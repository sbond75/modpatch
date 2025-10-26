;;; modpatch.el --- Patch-producing editing mode for game mods -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defgroup modpatch nil
  "Edit files as mod patches instead of writing them directly."
  :group 'tools)

(defcustom modpatch-associations-file
  (expand-file-name "modpatch-assoc.el" user-emacs-directory)
  "File where modpatch persists associations (base file -> patches, desired text)."
  :type 'file)

;; Internal global table:
;; key   = absolute base file path (string)
;; value = plist (:patches (list patchfile...) :desired desired-string)
(defvar modpatch--table (make-hash-table :test 'equal))


;;; Utility: file <-> string

(defun modpatch--read-file-as-string (file)
  "Return FILE contents as a string (no text properties)."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun modpatch--write-string-to-file (str file)
  "Write STR to FILE atomically."
  (let ((tmp (make-temp-file "modpatch-write-")))
    (with-temp-file tmp
      (insert str))
    (make-directory (file-name-directory file) t)
    (rename-file tmp file t)))

(defun modpatch--abs (file)
  (expand-file-name file))


;;; Persistent association save/load

(defun modpatch-save-associations ()
  "Persist `modpatch--table' to `modpatch-associations-file'."
  (with-temp-file modpatch-associations-file
    (let ((print-length nil)
          (print-level nil))
      (prin1
       (let (alist)
         (maphash (lambda (k v) (push (cons k v) alist)) modpatch--table)
         alist)
       (current-buffer)))))

(defun modpatch-load-associations ()
  "Load `modpatch--table' from `modpatch-associations-file', if it exists."
  (when (file-readable-p modpatch-associations-file)
    (with-temp-buffer
      (insert-file-contents modpatch-associations-file)
      (goto-char (point-min))
      (let ((alist (read (current-buffer))))
        (setq modpatch--table (make-hash-table :test 'equal))
        (dolist (cell alist)
          (puthash (car cell) (cdr cell) modpatch--table))))))

;; Load on first require.
(modpatch-load-associations)


;;; Patch / diff primitives (stubs you must finish)

(defun modpatch--generate-diff (base-text new-text base-name new-name)
  "Return a unified diff string transforming BASE-TEXT into NEW-TEXT.

BASE-NAME and NEW-NAME are filename labels used in the diff headers;
they do not have to be real files on disk, but they improve readability.

This function writes BASE-TEXT and NEW-TEXT to two temp files, calls
external `diff -u`, and returns its stdout. If there are no changes,
it returns an empty string."
  (let* ((tmp-base  (make-temp-file "modpatch-base-"))
         (tmp-new   (make-temp-file "modpatch-new-"))
         (diff-buf  (generate-new-buffer " *modpatch-diff*"))
         (status    nil)
         diff-str)
    (unwind-protect
        (progn
          ;; Write temp files
          (with-temp-file tmp-base
            (insert base-text))
          (with-temp-file tmp-new
            (insert new-text))

          ;; Call diff:
          ;;
          ;; We ask diff to pretend that the files are BASE-NAME and NEW-NAME
          ;; in the headers with the --label option. This produces stable,
          ;; cleaner patch headers instead of leaking temp file paths.
          ;;
          ;; diff exits 0 if no differences, 1 if differences, >1 on error.
          ;;
          (setq status
                (call-process
                 "diff" nil diff-buf nil
                 "-u"
                 (concat "--label=" base-name)
                 (concat "--label=" new-name)
                 tmp-base tmp-new))

          (cond
           ((or (eq status 0) (eq status 1))
            ;; status 0 => identical, diff is empty
            ;; status 1 => files differ, diff printed
            (with-current-buffer diff-buf
              (setq diff-str
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))

           (t
            (with-current-buffer diff-buf
              (let ((err-output
                     (buffer-substring-no-properties
                      (point-min) (point-max))))
                (error "modpatch--generate-diff: diff failed (%s): %s"
                       status err-output)))))

          ;; If files were identical, diff-str will be nil – normalize to "".
          (or diff-str ""))
      ;; cleanup
      (when (and tmp-base (file-exists-p tmp-base))
        (ignore-errors (delete-file tmp-base)))
      (when (and tmp-new (file-exists-p tmp-new))
        (ignore-errors (delete-file tmp-new)))
      (when (buffer-live-p diff-buf)
        (kill-buffer diff-buf)))))



(defun modpatch--apply-patch (base-text patch-text)
  "Apply PATCH-TEXT (a unified diff) to BASE-TEXT and return the patched result.

Signals an error if the patch cannot be applied cleanly.

Implementation notes:
1. We create a temp directory D.
2. We write BASE-TEXT into D/base.
3. We run `patch --forward --strip=0 --input=…` from that directory.
   --forward means: if a hunk is already applied, skip it instead of failing.
   You can remove --forward if you want strict failure on already-applied hunks.
4. We read back D/base and return it.

We ignore the paths inside the diff headers; we force patch to operate
on a known filename by normalizing the headers fed to patch.

To do that robustly, we rewrite PATCH-TEXT so that all file headers
(--- … / +++ …) refer to \"base\"."
  (let* ((tmp-dir   (make-temp-file "modpatch-patchdir-" t)) ;; t => directory
         (tmp-base  (expand-file-name "base" tmp-dir))
         (tmp-patch (expand-file-name "change.patch" tmp-dir))
         (patched-buf (generate-new-buffer " *modpatch-patched*"))
         (status    nil)
         cleaned-patch
         result-str)

    (unwind-protect
        (progn
          ;; Normalize patch headers to ensure they reference "base"
          ;;
          ;; Unified diff header lines look like:
          ;;   --- oldname\tTIMESTAMP
          ;;   +++ newname\tTIMESTAMP
          ;;
          ;; We rewrite both to "base".
          (setq cleaned-patch
                (with-temp-buffer
                  (insert patch-text)
                  (goto-char (point-min))
                  (while (re-search-forward
                          "^[+-]\\{3\\}\\s-+\\([^ \t\n]+\\)" nil t)
                    ;; Replace captured filename with "base"
                    (replace-match "base" t t nil 1))
                  (buffer-substring-no-properties
                   (point-min) (point-max))))

          ;; Write BASE-TEXT and cleaned PATCH-TEXT to disk
          (with-temp-file tmp-base
            (insert base-text))
          (with-temp-file tmp-patch
            (insert cleaned-patch))

          ;; Call patch
          ;;
          ;; --silent: reduce noise
          ;; --forward: don't fail if hunks already applied (optional; remove if unwanted)
          ;; --strip=0: do not strip leading components (we forced file name to "base")
          ;;
          ;; We run patch with `default-directory` = tmp-dir so it finds "base".
          (let ((default-directory tmp-dir))
            (setq status
                  (call-process
                   "patch" nil patched-buf nil
                   "--silent" "--forward" "--strip=0"
                   "--input" tmp-patch "base")))

          (unless (eq status 0)
            (with-current-buffer patched-buf
              (let ((err-output
                     (buffer-substring-no-properties
                      (point-min) (point-max))))
                (error "modpatch--apply-patch: patch failed (%s): %s"
                       status err-output))))

          ;; Read back the updated file
          (setq result-str
                (modpatch--read-file-as-string tmp-base))

          result-str)
      ;; cleanup
      (when (and tmp-base (file-exists-p tmp-base))
        (ignore-errors (delete-file tmp-base)))
      (when (and tmp-patch (file-exists-p tmp-patch))
        (ignore-errors (delete-file tmp-patch)))
      (when (and tmp-dir (file-exists-p tmp-dir))
        ;; delete-directory with recursive = t
        (ignore-errors (delete-directory tmp-dir t)))
      (when (buffer-live-p patched-buf)
        (kill-buffer patched-buf)))))


;;; Internal: look up and update association entries

(defun modpatch--get-entry (base-abs)
  "Return the association plist for BASE-ABS, creating one if needed."
  (or (gethash base-abs modpatch--table)
      (let ((entry (list :patches nil)))
        (puthash base-abs entry modpatch--table)
        entry)))

(defun modpatch--set-entry (base-abs entry)
  "Replace the association plist for BASE-ABS with ENTRY."
  (puthash base-abs entry modpatch--table))

(defun modpatch--find-patch-record (entry patch-file)
  "Return the patch record plist in ENTRY for PATCH-FILE, or nil."
  (cl-find patch-file (plist-get entry :patches)
           :key (lambda (rec) (plist-get rec :file))
           :test #'string-equal))

(defun modpatch--ensure-patch-record (entry patch-file)
  "Return (ENTRY' . RECORD) where RECORD is the patch record for PATCH-FILE.
If none existed, create one with keys :file PATCH-FILE, :desired nil, :base-ref nil.
Return the possibly-updated ENTRY and the record."
  (let* ((patch-file-abs (expand-file-name patch-file))
         (patches (plist-get entry :patches))
         (record (cl-find patch-file-abs patches
                          :key (lambda (rec) (plist-get rec :file))
                          :test #'string-equal)))
    (unless record
      (setq record (list :file patch-file-abs
                         :desired nil
                         :base-ref nil))
      (setq patches (append patches (list record)))
      (setf (plist-get entry :patches) patches))
    (cons entry record)))

(defun modpatch-add-patch-target (patch-file)
  "Associate PATCH-FILE with this buffer's base file and make it active.

PATCH-FILE is recorded in the global table. The current buffer's
contents become that patch's :desired immediately, and future saves
in patch-authoring mode will update only this PATCH-FILE."
  (interactive "FAdd patch file: ")
  (unless (bound-and-true-p modpatch--base-file)
    (user-error "Not in modpatch context"))

  (let* ((base modpatch--base-file)
         (abs  (expand-file-name patch-file))
         (entry (modpatch--get-entry base))
         (pair (modpatch--ensure-patch-record entry abs))
         (entry* (car pair))
         (record (cdr pair))
         (desired-now (buffer-substring-no-properties (point-min) (point-max))))
    ;; Update record's :desired to current buffer text.
    (setf (plist-get record :desired) desired-now)
    ;; Save back
    (modpatch--set-entry base entry*)
    (setq modpatch--active-patch-file abs)
    (modpatch-save-associations)
    (message "Added and activated patch target %s" abs)))


;;; Buffer-local state

(defvar-local modpatch--base-file nil
  "Absolute path of the base file for this buffer.")

(defvar-local modpatch--rebase-mode-p nil
  "Non-nil if this buffer is editing the true base on disk (modpatch-rebase-mode).
In that case we let Emacs save normally, then regenerate patches after save.")

(defvar-local modpatch--base-file nil
  "Absolute path of the base file for this buffer.")

(defvar-local modpatch--active-patch-file nil
  "Absolute path of the patch file this buffer is currently editing.")

(defvar-local modpatch--rebase-mode-p nil
  "Non-nil if this buffer is in rebase mode (editing the real base file).")


;;; Core save interception for patch mode

(defun modpatch--write-contents ()
  "Intercept save in patch-authoring mode (not rebase mode).

We only update the active patch file:
- Compute diff between the current on-disk base and the buffer text.
- Write that diff to the active patch file.
- Store :desired (buffer text) and :base-ref (current base text) in the patch record.
- Mark the buffer clean.

Return non-nil so Emacs believes the buffer is saved and does not write the base file."
  (when (and modpatch-mode (not modpatch--rebase-mode-p))
    (unless modpatch--active-patch-file
      (user-error "No active patch file for this buffer. Use modpatch-add-patch-target or modpatch-switch-to-patch-target."))
    (let* ((base modpatch--base-file)
           (active-patch modpatch--active-patch-file)
           (entry (modpatch--get-entry base))
           (pair (modpatch--ensure-patch-record entry active-patch))
           (entry* (car pair))
           (record (cdr pair))
           (base-text    (modpatch--read-file-as-string base))
           (desired-text (buffer-substring-no-properties (point-min) (point-max)))
           (diff-str (modpatch--generate-diff
                      base-text desired-text
                      (concat base ".orig")
                      (concat base ".modpatch"))))
      ;; write only this patch file
      (modpatch--write-string-to-file diff-str active-patch)
      ;; update record
      (setf (plist-get record :desired)  desired-text)
      (setf (plist-get record :base-ref) base-text)
      ;; save back and persist
      (modpatch--set-entry base entry*)
      (modpatch-save-associations)
      ;; mark clean
      (set-buffer-modified-p nil)
      t)))


;;; After-save hook for rebase mode

(defun modpatch--after-save-rebase ()
  "After saving the true base file on disk (in rebase mode):

For each patch record:
1. Try to merge upstream changes into that patch's :desired using a
   3-way merge of (:base-ref, new base, :desired). If merge succeeds
   with no conflicts, update :desired and :base-ref.

2. Regenerate that patch file by diffing the *new* base text against
   the (possibly merged) desired text.

3. If merge produced conflicts, we keep the old desired text, still
   regenerate the patch file from that unmerged desired, and emit a message."
  (when (and modpatch-mode modpatch--rebase-mode-p)
    (let* ((base modpatch--base-file)
           (entry (modpatch--get-entry base))
           (patches (plist-get entry :patches))
           (base-text-new (modpatch--read-file-as-string base)))
      (dolist (rec patches)
        (let* ((patch-file (plist-get rec :file))
               (desired-old (plist-get rec :desired))
               (base-old    (plist-get rec :base-ref)))

          ;; If we don't have desired/base-ref recorded yet, skip merge attempt.
          (let* ((merge-result
                  (if (and desired-old base-old)
                      (modpatch--three-way-merge base-old base-text-new desired-old)
                    ;; No merge possible; just carry the old desired forward.
                    (cons desired-old nil)))
                 (desired-merged (car merge-result))
                 (merge-reason   (cdr merge-result))
                 ;; If merge had conflict, keep old desired.
                 (desired-effective
                  (if merge-reason desired-old desired-merged))
                 ;; Compute new unified diff for this patch.
                 (diff-str (modpatch--generate-diff
                            base-text-new desired-effective
                            (concat base ".orig")
                            (concat base ".modpatch"))))

            ;; Write patch file
            (modpatch--write-string-to-file diff-str patch-file)

            ;; Update record if merge succeeded without conflict
            (unless merge-reason
              (setf (plist-get rec :desired)  desired-effective)
              (setf (plist-get rec :base-ref) base-text-new))

            ;; Emit message if merge conflict happened
            (when merge-reason
              (message "modpatch: merge for %s had conflicts (%s); kept old desired"
                       patch-file merge-reason)))))

      ;; Save updated association table and persist
      (modpatch--set-entry base entry)
      (modpatch-save-associations)
      (message "modpatch: all patches rebased and updated for %s" base))))



;;; Mode toggles

(defun modpatch-enter-rebase-mode ()
  "Switch current buffer into rebase mode.

Process:
1. For the active patch: run the patch-authoring save logic so its
   :desired and its .patch file are up to date with the current buffer.
2. Replace buffer contents with the real on-disk base file.
3. Enable after-save hook that updates all patches whenever you save.

After this call, you are looking at the true base file on disk."
  (interactive)
  (unless modpatch-mode
    (user-error "Enable modpatch-mode first"))
  (when modpatch--rebase-mode-p
    (user-error "Already in rebase mode"))

  ;; Step 1: flush current buffer changes into the active patch record and file.
  (modpatch--write-contents)

  ;; Step 2: show actual base on disk.
  (let* ((base modpatch--base-file)
         (base-text (modpatch--read-file-as-string base)))
    (modpatch--replace-buffer-with-text base-text))

  ;; Step 3: mark rebase mode and install after-save hook.
  (setq modpatch--rebase-mode-p t)
  (add-hook 'after-save-hook #'modpatch--after-save-rebase nil t)

  (message "modpatch: now editing base on disk; saving updates base, then rewrites all patches"))

(defun modpatch-exit-rebase-mode ()
  "Leave rebase mode and return to patch-authoring mode.

We restore the buffer to reflect the currently active patch file's
view of the base file.

Steps:
1. Force recompute desired text for the active patch from disk
   (using the latest .patch file) against the current base file.
2. Replace buffer contents with that desired text.
3. Disable the after-save hook and mark rebase-mode off."
  (interactive)
  (unless modpatch-mode
    (user-error "modpatch-mode is not active"))
  (unless modpatch--rebase-mode-p
    (user-error "Not currently in rebase mode"))
  (unless modpatch--active-patch-file
    (user-error "No active patch file to restore"))

  (let* ((base modpatch--base-file)
         (desired (modpatch--desired-for-patch
                   base modpatch--active-patch-file t)))
    (modpatch--replace-buffer-with-text desired))

  ;; Disable rebase behavior.
  (remove-hook 'after-save-hook #'modpatch--after-save-rebase t)
  (setq modpatch--rebase-mode-p nil)

  (message "modpatch: now editing only patch %s; saving updates that patch"
           modpatch--active-patch-file))


;;; Auto-activation on find-file

(defun modpatch--apply-all-patches-to-base (base-abs entry &optional force-recompute)
  "Return the desired (patched) text for BASE-ABS.

ENTRY is the plist stored in `modpatch--table` for BASE-ABS.

If FORCE-RECOMPUTE is non-nil, we ignore any cached :desired in ENTRY
and instead rebuild the desired text by applying the current on-disk
patch files in :patches to the current on-disk base file. We then
update ENTRY's :desired with that rebuilt text.

If FORCE-RECOMPUTE is nil and ENTRY already has :desired, we reuse it.

In all cases, we ensure `modpatch--table` is updated with the final
:desired, and we persist associations so future sessions see the
same state."
  (let* ((desired (plist-get entry :desired))
         (patches (plist-get entry :patches)))
    (when (or force-recompute (null desired))
      ;; Start from the current base file on disk.
      (let ((tmp (modpatch--read-file-as-string base-abs)))
        ;; Apply each patch file in order. The result after all patches
        ;; is the desired modded view.
        (dolist (pf patches)
          (let ((patch-text (modpatch--read-file-as-string pf)))
            (setq tmp (modpatch--apply-patch tmp patch-text))))
        (setq desired tmp))

      ;; Update entry with the fresh desired state and write back.
      (setf (plist-get entry :desired) desired)
      (modpatch--set-entry base-abs entry)
      (modpatch-save-associations))

    desired))

(defun modpatch-maybe-activate ()
  "If visiting a tracked base file, show the desired state for the first patch
and enable modpatch-mode. Does nothing if the file is unknown or has no patches."
  (when buffer-file-name
    (let* ((base (expand-file-name buffer-file-name))
           (entry (gethash base modpatch--table)))
      (when entry
        (let* ((patches (plist-get entry :patches)))
          (when (null patches)
            ;; No patches registered: nothing to activate.
            (cl-return-from modpatch-maybe-activate))
          (let* ((first-rec (car patches))
                 (patch-file (plist-get first-rec :file))
                 ;; Force recompute so buffer reflects latest .patch on disk.
                 (desired (modpatch--desired-for-patch base patch-file t)))
            (modpatch--replace-buffer-with-text desired)
            (setq modpatch--base-file base)
            (setq modpatch--active-patch-file patch-file)
            (modpatch-mode 1)))))))

(add-hook 'find-file-hook #'modpatch-maybe-activate)


;;; Minor modes

(defvar modpatch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m a") #'modpatch-add-patch-target)
    (define-key map (kbd "C-c m p") #'modpatch-switch-to-patch-target)
    (define-key map (kbd "C-c m r") #'modpatch-enter-rebase-mode)
    (define-key map (kbd "C-c m R") #'modpatch-exit-rebase-mode)
    (define-key map (kbd "C-c m s") #'modpatch-save-associations)
    map)
  "Keymap for `modpatch-mode'.")

(define-minor-mode modpatch-mode
  "Minor mode to edit a file as a mod patch instead of saving it directly.

When modpatch-mode is active and not in rebase mode:
- The buffer is assumed to correspond to a 'base file' on disk,
  stored in `modpatch--base-file'.
- The visible contents are the desired modded result.
- Saving (C-x C-s) does NOT write to the base file. Instead it
  regenerates one or more unified diff patch files and marks the
  buffer clean without touching the base file on disk.
  This is implemented via `write-contents-functions', which allows a
  buffer to say \"I handled saving myself\" and stop Emacs from
  writing the visited file. :contentReference[oaicite:6]{index=6}

When `modpatch-enter-rebase-mode' is called:
- We toggle `modpatch--rebase-mode-p' so that saves write the base
  file normally, and after-save hooks recompute patches so they still
  produce the recorded desired state.

Use `modpatch-exit-rebase-mode' to switch back."
  :lighter " ModPatch"
  :keymap modpatch-mode-map
  (if modpatch-mode
      ;; enabling
      (progn
        (unless buffer-file-name
          (user-error "modpatch-mode needs a file-visiting buffer"))
        (setq modpatch--base-file (modpatch--abs buffer-file-name))
        ;; install buffer-local write-contents-functions hook
        (add-hook 'write-contents-functions #'modpatch--write-contents nil t))
    ;; disabling
    (remove-hook 'write-contents-functions #'modpatch--write-contents t)
    (remove-hook 'after-save-hook #'modpatch--after-save-rebase t)
    (setq modpatch--rebase-mode-p nil)))

(defun modpatch--replace-buffer-with-text (text)
  "Replace entire buffer contents with TEXT, keeping point at start,
and mark buffer unmodified."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert text)
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun modpatch--patch-targets-for-current-buffer ()
  "Return the list of patch file paths associated with this buffer's base file.
Signals an error if not in modpatch-mode or no association exists."
  (unless (and (bound-and-true-p modpatch-mode)
               (bound-and-true-p modpatch--base-file))
    (user-error "Not in modpatch-mode for a tracked base file"))
  (let* ((base modpatch--base-file)
         (entry (gethash base modpatch--table)))
    (unless entry
      (user-error "No modpatch association found for %s" base))
    (let ((patches (plist-get entry :patches)))
      (unless patches
        (user-error "No patch targets are registered for %s" base))
      patches)))


(defun modpatch--all-patch-files-for-base (base-abs)
  "Return list of patch file paths associated with BASE-ABS."
  (let* ((entry (gethash base-abs modpatch--table)))
    (unless entry
      (user-error "No modpatch entry for %s" base-abs))
    (mapcar (lambda (rec) (plist-get rec :file))
            (plist-get entry :patches))))

(defun modpatch-switch-to-patch-target ()
  "Pick one of the associated patch files and make it active in this buffer.

After selection:
1. The buffer becomes that patch's desired state.
2. Future saves will update only that one patch file.
3. Other patches are untouched.

This does not enter or exit rebase mode; it just changes which patch
you're working on."
  (interactive)
  (unless (and (bound-and-true-p modpatch-mode)
               (bound-and-true-p modpatch--base-file))
    (user-error "Not in modpatch-mode on a tracked base file"))

  (let* ((base modpatch--base-file)
         (patches (modpatch--all-patch-files-for-base base))
         (choice (cond
                  ((null patches)
                   (user-error "No patch targets registered for %s" base))
                  ((= (length patches) 1)
                   (car patches))
                  (t
                   (completing-read
                    "Activate patch: " patches nil t nil nil (car patches))))))
    ;; Compute / load desired for this patch, force recompute from disk
    ;; to ensure it's up to date with the .patch file as it exists now.
    (let ((desired (modpatch--desired-for-patch base choice t)))
      (modpatch--replace-buffer-with-text desired))
    ;; Mark active patch
    (setq modpatch--active-patch-file (expand-file-name choice))
    (message "modpatch: now editing patch %s" modpatch--active-patch-file)))

(defun modpatch--desired-for-patch (base-abs patch-file &optional force-recompute)
  "Return desired text for PATCH-FILE against BASE-ABS.

If FORCE-RECOMPUTE is non-nil, ignore cached :desired and rebuild by
applying PATCH-FILE (read from disk) to the current base file text.

In both cases, update the patch record's :desired and :base-ref in the table,
and persist."
  (let* ((entry (modpatch--get-entry base-abs))
         (pair (modpatch--ensure-patch-record entry patch-file))
         (entry* (car pair))
         (record (cdr pair))
         (desired (plist-get record :desired)))
    (when (or force-recompute (null desired))
      (let* ((base-text  (modpatch--read-file-as-string base-abs))
             (patch-text (modpatch--read-file-as-string (plist-get record :file)))
             (rebuilt    (modpatch--apply-patch base-text patch-text)))
        (setq desired rebuilt)
        (setf (plist-get record :desired)  desired)
        (setf (plist-get record :base-ref) base-text)))
    ;; save entry updates and persist
    (modpatch--set-entry base-abs entry*)
    (modpatch-save-associations)
    (plist-get record :desired)))

(defun modpatch--contains-merge-conflict-markers-p (text)
  "Return non-nil if TEXT seems to contain diff3-style conflict markers."
  (or (string-match-p "^<<<<<<< " text)
      (string-match-p "^=======" text)
      (string-match-p "^>>>>>>> " text)))

(defun modpatch--three-way-merge (base-old base-new desired-old)
  "Attempt a 3-way merge of BASE-OLD (common ancestor),
BASE-NEW (upstream after edits), and DESIRED-OLD (our modded view).

Return (MERGED . nil) on success, where MERGED is the merged text
with upstream changes folded in.

Return (DESIRED-OLD . REASON) on conflict, where REASON is a string
describing why merge failed. In that case the caller should keep
DESIRED-OLD as-is.

This implementation shells out to diff3 -m."
  (let* ((tmp-dir   (make-temp-file "modpatch-merge-" t))
         (f-anc     (expand-file-name "A_ancestor" tmp-dir))
         (f-up      (expand-file-name "B_upstream" tmp-dir))
         (f-ours    (expand-file-name "C_desired"  tmp-dir))
         (merge-buf (generate-new-buffer " *modpatch-merge*"))
         merged-text reason status)
    (unwind-protect
        (progn
          ;; write inputs
          (with-temp-file f-anc (insert base-old))
          (with-temp-file f-up  (insert base-new))
          (with-temp-file f-ours(insert desired-old))

          ;; Call diff3:
          ;;   diff3 -m ours ancestor upstream
          ;; We want: merge desired-old with base-new using base-old as common ancestor.
          ;; diff3's -m output: conflict markers if conflicts.
          (setq status
                (call-process "diff3" nil merge-buf nil
                              "-m" f-ours f-anc f-up))

          ;; status 0 or 1 both can mean "merge complete" depending on system;
          ;; we'll detect success by scanning for conflict markers.
          (with-current-buffer merge-buf
            (setq merged-text
                  (buffer-substring-no-properties (point-min) (point-max))))

          (if (modpatch--contains-merge-conflict-markers-p merged-text)
              (setq reason "merge conflict")
            (setq reason nil))

          (cons merged-text reason))
      ;; cleanup
      (when (and tmp-dir (file-exists-p tmp-dir))
        (ignore-errors (delete-directory tmp-dir t)))
      (when (buffer-live-p merge-buf)
        (kill-buffer merge-buf)))))

(provide 'modpatch)

;;; modpatch.el ends here
