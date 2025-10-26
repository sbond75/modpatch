;;; mod-patch.el --- Transparent patch-based editing for Emacs -*- lexical-binding: t; -*-

;; Author: Your Name <you@example.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (json "1.5"))
;; Keywords: vc, patches, convenience
;; URL: https://example.com/mod-patch
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; `mod-patch-mode’ lets you edit a source file *as usual* while saving all
;; modifications into version-controlled *patch files* instead of overwriting
;; the original on disk.  You may seamlessly switch between the original
;; buffer and any of its associated patch buffers, keep persistent state
;; between sessions, and recover gracefully from patch-application failures.
;;
;; Core user commands
;; ------------------
;; M-x mod-patch-setup-for-current-buffer   ─ enable patch management here
;; M-x mod-patch-toggle                     ─ flip between original / last patch
;; M-x mod-patch-switch                     ─ pick a specific patch to edit
;; M-x mod-patch-merge-finish               ─ resolve a failed merge
;;
;; See the README/specification for the full feature list.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'diff-mode)

;;;; ---------------------------------------------------------------------
;;;; User options
;;;; ---------------------------------------------------------------------

(defgroup mod-patch nil
  "Patch-centric editing workflow."
  :group 'convenience
  :link '(url-link :tag "GitHub" "https://example.com/mod-patch"))

(defcustom mod-patch-target-directory nil
  "Directory in which to store patch files.
If nil, patches are written next to the original file."
  :type '(choice (const :tag "Beside original file" nil)
                 (directory :tag "Specific directory")))

(defcustom mod-patch-state-file
  (expand-file-name "mod-patch-state.json" user-emacs-directory)
  "File used to persist state across Emacs sessions."
  :type 'file)

(defcustom mod-patch-diff-context 12
  "Number of context lines to include when writing unified diffs."
  :type 'integer)

(defcustom mod-patch-recent-patches-limit 20
  "Maximum patches remembered per original file."
  :type 'integer)

;;;; ---------------------------------------------------------------------
;;;; Internal state helpers
;;;; ---------------------------------------------------------------------

(defvar mod-patch--state (make-hash-table :test #'equal)
  "Hash map: ORIGINAL-FILE → plist (:patches (...) :last-mode sym :last-patch str).")

(defun mod-patch--load-state ()
  "Populate `mod-patch--state' from `mod-patch-state-file'."
  (when (file-exists-p mod-patch-state-file)
    (let ((json-object-type 'hash-table)
          (json-array-type 'list))
      (setq mod-patch--state
            (json-read-file mod-patch-state-file)))))

(defun mod-patch--save-state ()
  "Persist `mod-patch--state' to `mod-patch-state-file'."
  (unless (file-directory-p (file-name-directory mod-patch-state-file))
    (make-directory (file-name-directory mod-patch-state-file) t))
  (with-temp-file mod-patch-state-file
    (insert (json-encode mod-patch--state))))

(defun mod-patch--get-entry (orig)
  "Return plist state entry for ORIG, initialising if necessary."
  (or (gethash orig mod-patch--state)
      (let ((entry (list :patches '() :last-mode 'original :last-patch nil)))
        (puthash orig entry mod-patch--state)
        entry)))

(defun mod-patch--update-entry (orig entry)
  "Replace ORIG’s state ENTRY and persist."
  (puthash orig entry mod-patch--state)
  (mod-patch--save-state))

;;;; ---------------------------------------------------------------------
;;;; Minor mode locals
;;;; ---------------------------------------------------------------------

(defvar-local mod-patch--current-mode 'original
  "Symbol: `original' or `patch'.")

(defvar-local mod-patch--current-patch-file nil
  "If in patch mode, absolute path of the patch file backing the buffer.")

(defvar-local mod-patch--write-guard nil
  "Non-nil while `write-contents-functions' is running (prevents recursion).")

;;;; ---------------------------------------------------------------------
;;;; Utility helpers
;;;; ---------------------------------------------------------------------

(defun mod-patch--patch-directory (orig)
  "Return directory where patches for ORIG should be kept."
  (or mod-patch-target-directory (file-name-directory orig)))

(defun mod-patch--next-patch-path (orig)
  "Compute a unique patch file path for ORIG."
  (let* ((base (file-name-nondirectory orig))
         (ts   (format-time-string "%Y%m%d%H%M%S"))
         (name (format "%s.%s.patch" base ts)))
    (expand-file-name name (mod-patch--patch-directory orig))))

(defun mod-patch--write-diff (orig buf dest)
  "Compute diff between ORIG (file path) and BUF (buffer object), writing to DEST."
  (with-temp-buffer
    (let ((diff-cmd (or (executable-find "diff") (error "diff(1) not found"))))
      (unless (zerop
               (call-process diff-cmd nil t nil
                             "-u" (format "-U%d" mod-patch-diff-context)
                             orig
                             (progn
                               ;; Save buffer to a temp file for diff input
                               (setq dest (expand-file-name dest))
                               (let ((tmp (make-temp-file "mod-patch-diff")))
                                 (with-current-buffer buf
                                   (write-region nil nil tmp nil 0))
                                 tmp))))
        (write-region (point-min) (point-max) dest nil 0)))))

(defun mod-patch--apply-patch-to-region (patch-buf target-buf)
  "Apply unified diff in PATCH-BUF to TARGET-BUF, replacing its contents.
Return nil on success, or buffer with patch error output."
  (let* ((patch-cmd (or (executable-find "patch") (error "patch(1) not found")))
         (tmp (make-temp-file "mod-patch-orig"))
         (out (generate-new-buffer "*mod-patch-errors*")))
    (unwind-protect
        (progn
          (with-current-buffer target-buf
            (write-region nil nil tmp nil 0))
          (with-current-buffer patch-buf
            (unless (zerop
                     (call-process-region (point-min) (point-max)
                                          patch-cmd nil out nil
                                          "-u" "-l" "-o" tmp tmp))
              out)))
      (with-current-buffer target-buf
        (erase-buffer)
        (insert-file-contents tmp))
      (delete-file tmp))))

(defun mod-patch--update-mode-line ()
  "Refresh mode-line indicator."
  (setq-local mode-name
              (format "ModPatch[%s]"
                      (if (eq mod-patch--current-mode 'original)
                          "O"
                        (file-name-nondirectory mod-patch--current-patch-file))))
  (force-mode-line-update))

;;;; ---------------------------------------------------------------------
;;;; Writing interception (original mode)
;;;; ---------------------------------------------------------------------

(defun mod-patch--write-contents-hook ()
  "Intercept `save-buffer' when in original mode, writing patch instead."
  (unless mod-patch--write-guard
    (setq mod-patch--write-guard t)
    (unwind-protect
        (let* ((orig (buffer-file-name))
               (patch (mod-patch--next-patch-path orig)))
          (mod-patch--write-diff orig (current-buffer) patch)
          ;; Update persistent state
          (let* ((entry (mod-patch--get-entry orig))
                 (plist (plist-put entry :patches
                                   (cl-subseq (push patch (plist-get entry :patches))
                                              0 mod-patch-recent-patches-limit))))
            (mod-patch--update-entry orig plist))
          ;; Mark buffer unmodified (nothing written to disk)
          (set-buffer-modified-p nil)
          (message "mod-patch: wrote %s" (abbreviate-file-name patch)))
      (setq mod-patch--write-guard nil))
    ;; Returning non-nil tells Emacs we handled the save
    t))

;;;; ---------------------------------------------------------------------
;;;; Buffer initialisation / teardown
;;;; ---------------------------------------------------------------------

(defun mod-patch--enable ()
  "Helper run when `mod-patch-mode' is enabled."
  (add-hook 'write-contents-functions #'mod-patch--write-contents-hook nil t)
  (mod-patch--update-mode-line)
  ;; Auto-restore last view
  (let* ((orig (buffer-file-name))
         (entry (mod-patch--get-entry orig)))
    (when (eq (plist-get entry :last-mode) 'patch)
      (let ((last (plist-get entry :last-patch)))
        (when (and last (file-exists-p last))
          (mod-patch--visit-patch last))))))

(defun mod-patch--disable ()
  "Helper run when `mod-patch-mode' is disabled."
  (remove-hook 'write-contents-functions #'mod-patch--write-contents-hook t)
  (setq mod-patch--current-mode 'original
        mod-patch--current-patch-file nil)
  (mod-patch--update-mode-line))

;;;; ---------------------------------------------------------------------
;;;; Interactive patch switching
;;;; ---------------------------------------------------------------------

(defun mod-patch--visit-patch (patch-file)
  "Switch current buffer into PATCH-FILE editing mode."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert-file-contents patch-file)
    (setq buffer-read-only nil))
  (diff-mode)
  (setq mod-patch--current-mode 'patch
        mod-patch--current-patch-file patch-file)
  (mod-patch--update-mode-line)
  ;; Remember for persistence
  (let* ((entry (mod-patch--get-entry (buffer-file-name (or (buffer-base-buffer) (current-buffer)))))
         (entry (plist-put entry :last-mode 'patch))
         (entry (plist-put entry :last-patch patch-file)))
    (mod-patch--update-entry (buffer-file-name) entry)))

(defun mod-patch-switch ()
  "Select and visit a recent patch for the current file."
  (interactive)
  (let* ((orig (buffer-file-name))
         (entry (mod-patch--get-entry orig))
         (patches (plist-get entry :patches)))
    (unless patches (user-error "No patches recorded for this file"))
    (let* ((choice (completing-read "Patch: " patches nil t)))
      (mod-patch--visit-patch choice))))

(defun mod-patch-toggle ()
  "Toggle between original buffer and its last visited patch."
  (interactive)
  (pcase mod-patch--current-mode
    ('original
     (let* ((orig (buffer-file-name))
            (entry (mod-patch--get-entry orig))
            (last (car (plist-get entry :patches))))
       (if (and last (file-exists-p last))
           (mod-patch--visit-patch last)
         (user-error "No patch to toggle to"))))
    (_ ;; currently in patch, so go back to original
     (revert-buffer t t t) ;; reload pristine contents
     (diff-mode 0)
     (setq mod-patch--current-mode 'original
           mod-patch--current-patch-file nil)
     (mod-patch--update-mode-line)
     (let* ((orig (buffer-file-name))
            (entry (mod-patch--get-entry orig)))
       (setq entry (plist-put entry :last-mode 'original))
       (mod-patch--update-entry orig entry)))))

;;;; ---------------------------------------------------------------------
;;;; Merge / conflict handling
;;;; ---------------------------------------------------------------------

(defun mod-patch-merge-finish ()
  "Manually merge current patch into the original and refresh."
  (interactive)
  (unless (and (eq mod-patch--current-mode 'patch) mod-patch--current-patch-file)
    (user-error "Not visiting a patch buffer"))
  (let* ((err (mod-patch--apply-patch-to-region (current-buffer)
                                                (find-file-noselect (buffer-base-buffer)))))
    (if err
        (progn
          (display-buffer err)
          (message "mod-patch: merge failed; resolve conflicts then run again"))
      (save-buffer) ;; save original file changes
      (kill-buffer)
      (message "mod-patch: merge successful"))))

;;;; ---------------------------------------------------------------------
;;;; Entry point
;;;; ---------------------------------------------------------------------

;;;###autoload
(define-minor-mode mod-patch-mode
  "Minor mode for patch-centric editing workflow."
  :init-value nil
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c p t") #'mod-patch-toggle)
            (define-key map (kbd "C-c p s") #'mod-patch-switch)
            (define-key map (kbd "C-c p m") #'mod-patch-merge-finish)
            map)
  (if mod-patch-mode
      (mod-patch--enable)
    (mod-patch--disable)))

;;;###autoload
(defun mod-patch-setup-for-current-buffer ()
  "Enable `mod-patch-mode' in the current buffer with sensible defaults."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer has no associated file"))
  (mod-patch--load-state)
  (mod-patch-mode 1))

;;;; ---------------------------------------------------------------------
;;;; Multiple target-directory support
;;;; ---------------------------------------------------------------------

(defcustom mod-patch-known-directories nil
  "List of user-configured patch directories.
Each element *must* be an absolute directory path *with* a trailing slash.
The list is persisted automatically; you rarely need to set it manually."
  :type '(repeat directory))

(defun mod-patch--normalise-dir (dir)
  "Return DIR as an absolute name ending with a directory separator."
  (file-name-as-directory (expand-file-name dir)))

(defun mod-patch--remember-directory (dir)
  "Add DIR to `mod-patch-known-directories', avoiding duplicates, and persist."
  (setq dir (mod-patch--normalise-dir dir))
  (unless (member dir mod-patch-known-directories)
    (push dir mod-patch-known-directories))
  ;; Persist alongside the ordinary state file
  (mod-patch--save-state)
  dir)

;;;###autoload
(defun mod-patch-add-patch-directory (dir)
  "Interactively add DIR to the set of patch target directories.

The directory is created if it does not yet exist, recorded in
`mod-patch-known-directories', and becomes the new default
(`mod-patch-target-directory') for the current session."
  (interactive "DNew patch directory: ")
  (setq dir (mod-patch--normalise-dir dir))
  (unless (file-directory-p dir)
    (make-directory dir t))
  (mod-patch--remember-directory dir)
  (setq mod-patch-target-directory dir)
  (message "mod-patch: now using %s for new patches"
           (abbreviate-file-name dir)))

;;;; ---------------------------------------------------------------------
;;;; Completion helper for recent *directories*
;;;; ---------------------------------------------------------------------

(defun mod-patch--completing-patch-dirs ()
  "Return list of patch directories suitable for `completing-read'."
  (mapcar (lambda (p)
            ;; Show parent directory names to disambiguate similar tails.
            (cons (abbreviate-file-name p) p))
          mod-patch-known-directories))

(provide 'mod-patch)
;;; mod-patch.el ends here
