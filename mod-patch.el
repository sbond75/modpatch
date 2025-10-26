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
  "Return unified diff string that would transform BASE-TEXT into NEW-TEXT.
BASE-NAME and NEW-NAME are just labels that will appear in the diff headers."
  ;; Simplest approach: write both to temp files, call external diff -u.
  ;; You must implement this for your platform.
  (error "modpatch--generate-diff: not implemented"))

(defun modpatch--apply-patch (base-text patch-text)
  "Apply PATCH-TEXT (a unified diff) to BASE-TEXT and return the patched result.
Signal an error if patch cannot be applied cleanly."
  ;; Simplest approach: write BASE-TEXT to tmp file A, run `patch` with PATCH-TEXT
  ;; into tmp file B, read B back. Or call into `epatch-buffer` in a temp buffer.
  ;; See Emacs commands `epatch-buffer` / `diff-apply-hunk`. :contentReference[oaicite:4]{index=4}
  (error "modpatch--apply-patch: not implemented"))


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
  "Switch current buffer into 'rebase' mode:
we now edit & save the real base file, and patch files get auto-regenerated."
  (interactive)
  (unless modpatch-mode
    (user-error "Enable modpatch-mode first"))
  (setq modpatch--rebase-mode-p t)
  (add-hook 'after-save-hook #'modpatch--after-save-rebase nil t)
  (message "modpatch: now editing base; saving will rewrite patches"))

(defun modpatch-exit-rebase-mode ()
  "Return to patch-authoring mode: saving writes patches, not the base file."
  (interactive)
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
  "If the file we just visited has modpatch metadata, rewrite buffer contents
to show the desired (patched) state and enable `modpatch-mode'."
  (when buffer-file-name
    (let* ((base (modpatch--abs buffer-file-name))
           (entry (gethash base modpatch--table)))
      (when entry
        ;; Replace buffer text with desired (patched) text.
        (let ((desired (modpatch--apply-all-patches-to-base base entry)))
          (erase-buffer)
          (insert desired)
          (goto-char (point-min))
          ;; Mark unmodified (this is our working view right now).
          (set-buffer-modified-p nil))
        ;; Enable modpatch-mode for this buffer.
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

(provide 'modpatch)

;;; modpatch.el ends here
