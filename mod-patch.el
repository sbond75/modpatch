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
  (or (gethash base-abs modpatch--table)
      (let ((entry (list :patches nil :desired nil)))
        (puthash base-abs entry modpatch--table)
        entry)))

(defun modpatch--set-entry (base-abs entry)
  (puthash base-abs entry modpatch--table))

(defun modpatch-add-patch-target (patch-file)
  "Interactively add PATCH-FILE as a patch target for the current buffer's base."
  (interactive "FAdd patch file: ")
  (unless (bound-and-true-p modpatch--base-file)
    (user-error "Not in modpatch context"))
  (let* ((base modpatch--base-file)
         (abs (modpatch--abs patch-file))
         (entry (modpatch--get-entry base))
         (plist (copy-sequence entry))
         (targets (plist-get plist :patches)))
    (unless (member abs targets)
      (setq targets (append targets (list abs))))
    (setf (plist-get plist :patches) targets)
    (modpatch--set-entry base plist)
    (modpatch-save-associations)
    (message "Added patch target %s" abs)))


;;; Buffer-local state

(defvar-local modpatch--base-file nil
  "Absolute path of the base file for this buffer.")

(defvar-local modpatch--rebase-mode-p nil
  "Non-nil if this buffer is editing the true base on disk (modpatch-rebase-mode).
In that case we let Emacs save normally, then regenerate patches after save.")


;;; Core save interception for patch mode

(defun modpatch--write-contents ()
  "Buffer-local hook for `write-contents-functions'.
Instead of saving the buffer to the visited file, write/refresh patch files.
Return non-nil to tell Emacs the save is handled. :contentReference[oaicite:5]{index=5}"
  (when (and modpatch-mode (not modpatch--rebase-mode-p))
    (let* ((base modpatch--base-file)
           (entry (modpatch--get-entry base))
           ;; What base currently looks like on disk:
           (base-text (modpatch--read-file-as-string base))
           ;; What user wants (this buffer):
           (desired-text (buffer-substring-no-properties (point-min) (point-max)))
           (patches (plist-get entry :patches)))
      ;; Update :desired
      (let ((new-entry (copy-sequence entry)))
        (setf (plist-get new-entry :desired) desired-text)
        (modpatch--set-entry base new-entry))

      ;; Compute diff and update every patch target.
      (let ((diff-str (modpatch--generate-diff
                       base-text desired-text
                       (concat base ".orig")
                       (concat base ".modpatch"))))
        (dolist (pf patches)
          (modpatch--write-string-to-file diff-str pf)))

      ;; Persist associations.
      (modpatch-save-associations)

      ;; Pretend buffer is saved.
      (set-buffer-modified-p nil)
      t)))


;;; After-save hook for rebase mode

(defun modpatch--after-save-rebase ()
  "After saving the true base file on disk, regenerate patches from :desired."
  (when (and modpatch-mode modpatch--rebase-mode-p)
    (let* ((base modpatch--base-file)
           (entry (modpatch--get-entry base))
           (desired (plist-get entry :desired))
           (patches (plist-get entry :patches))
           ;; new base on disk now:
           (base-text (modpatch--read-file-as-string base)))
      (unless desired
        ;; If there's no :desired stored, nothing to do.
        (message "modpatch: no desired content recorded for %s" base)
        (cl-return-from modpatch--after-save-rebase))

      ;; Recompute diff (desired vs new base), overwrite patch files.
      (let ((diff-str (modpatch--generate-diff
                       base-text desired
                       (concat base ".orig")
                       (concat base ".modpatch"))))
        (dolist (pf patches)
          (modpatch--write-string-to-file diff-str pf)))
      (message "modpatch: patches updated for %s" base))))


;;; Mode toggles

(defun modpatch-enter-rebase-mode ()
  "Switch current buffer into rebase mode.

Steps:
1. Capture/commit the current desired modded view so it is not lost.
   This updates the association entry's :desired and rewrites patch files.
2. Load the true on-disk base file contents into the buffer.
3. Enable after-save hook so that future saves rewrite patches.

After this call, the buffer is literally the base file on disk.
Saving (C-x C-s) writes the base file, then patches are recalculated."
  (interactive)
  (unless modpatch-mode
    (user-error "Enable modpatch-mode first"))
  (when modpatch--rebase-mode-p
    (user-error "Already in rebase mode"))

  ;; Step 1: commit desired state and refresh patch files.
  ;; We reuse the same logic as `modpatch--write-contents`, but we call it
  ;; directly instead of going through save-buffer. The hook returns non-nil
  ;; to indicate it 'handled' the save. We don't care about that here.
  (modpatch--write-contents)

  ;; Step 2: replace buffer text with the true base file on disk.
  (let* ((base modpatch--base-file)
         (base-text (modpatch--read-file-as-string base)))
    (modpatch--replace-buffer-with-text base-text))

  ;; Step 3: flip mode state to rebase mode and install after-save hook.
  (setq modpatch--rebase-mode-p t)
  (add-hook 'after-save-hook #'modpatch--after-save-rebase nil t)

  (message "modpatch: now editing base on disk; saving updates base, then rewrites patches"))

(defun modpatch-exit-rebase-mode ()
  "Leave rebase mode and return to patch-authoring mode.

Steps:
1. Make sure the association table still has a correct :desired.
   If :desired is missing we recompute it by applying all patches to
   the current on-disk base file.
2. Replace the buffer text with that desired (patched) view.
3. Remove the after-save hook and mark this buffer as patch-authoring,
   so saving regenerates patch files instead of touching the base file."
  (interactive)
  (unless modpatch-mode
    (user-error "modpatch-mode is not active"))
  (unless modpatch--rebase-mode-p
    (user-error "Not currently in rebase mode"))

  (let* ((base modpatch--base-file)
         (entry (modpatch--get-entry base))
         ;; Ensure :desired is populated/consistent.
         (desired
          (modpatch--apply-all-patches-to-base base entry)))
    ;; Step 2: restore buffer to desired content.
    (modpatch--replace-buffer-with-text desired))

  ;; Step 3: leave rebase mode.
  (remove-hook 'after-save-hook #'modpatch--after-save-rebase t)
  (setq modpatch--rebase-mode-p nil)

  (message "modpatch: now editing modded view; saving writes patches only"))


;;; Auto-activation on find-file

(defun modpatch--apply-all-patches-to-base (base-abs entry)
  "Return desired text for BASE-ABS. If entry already has :desired, reuse it.
Otherwise, apply each patch in :patches to the current base to compute it,
store it back in :desired, and return it."
  (let* ((desired (plist-get entry :desired))
         (patches (plist-get entry :patches)))
    (unless desired
      (let ((tmp (modpatch--read-file-as-string base-abs)))
        (dolist (pf patches)
          (let ((patch-text (modpatch--read-file-as-string pf)))
            (setq tmp (modpatch--apply-patch tmp patch-text))))
        (setq desired tmp)
        (setf (plist-get entry :desired) desired)
        (modpatch--set-entry base-abs entry)))
    desired))

(defun modpatch-maybe-activate ()
  "If visiting a file with modpatch metadata, replace buffer contents
with the patched view and enable modpatch-mode."
  (when buffer-file-name
    (let* ((base (modpatch--abs buffer-file-name))
           (entry (gethash base modpatch--table)))
      (when entry
        (let ((desired (modpatch--apply-all-patches-to-base base entry)))
          (modpatch--replace-buffer-with-text desired))
        (modpatch-mode 1)))))

(add-hook 'find-file-hook #'modpatch-maybe-activate)


;;; Minor modes

(defvar modpatch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m a") #'modpatch-add-patch-target)
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

(provide 'modpatch)

;;; modpatch.el ends here
