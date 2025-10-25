;;; mod-patch.el --- Robust live patch authoring for game mods -*- lexical-binding:t -*-

;; Copyright (C) 2025  Your Name
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'subr-x)

(defgroup mod-patch nil
  "Edit files while saving changes to .patch files."
  :group 'tools)

(defcustom mod-patch-recent-limit 10
  "Maximum number of (ORIGINAL . PATCH) pairs remembered."
  :type 'integer :group 'mod-patch)

;;;; ---------------------------------------------------------------------
;;;; Internals: file/patch bookkeeping
;;;; ---------------------------------------------------------------------

(defvar-local mod-patch-target-directory nil
  "Directory where the .patch file will be written for this buffer.
If nil, defaults to the original file's directory.")

(defconst mod-patch--header-re
  "^---- MOD-PATCH base-sha1:\\([0-9a-f]+\\) ----$")

(defun mod-patch--sha1-file (file)
  "Return SHA-1 of FILE's literal bytes."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (secure-hash 'sha1 (current-buffer))))

(defun mod-patch--insert-header (sha1)
  (insert (format "---- MOD-PATCH base-sha1:%s ----\n" sha1)))

(defun mod-patch--extract-header-sha1 ()
  (save-excursion
    (goto-char (point-min))
    (when (looking-at mod-patch--header-re)
      (match-string 1))))

(defun mod-patch--write-diff (orig tmp patch-file)
  "Build large-context diff between ORIG and TMP into PATCH-FILE, preserving header."
  (let ((buf (get-buffer-create "*mod-patch-diff*")))
    (with-current-buffer buf
      (erase-buffer))
    ;; -U12 = 12 context lines; -N handles new/delete; -u is unified
    (let ((status (call-process "diff" nil buf nil "-u" "-U12" "-N" orig tmp)))
      (cond
       ;; 0 → files identical: delete any existing patch
       ((= status 0)
        (when (file-exists-p patch-file)
          (delete-file patch-file)))
       ;; 1 → diff produced: prepend header and write file
       ((= status 1)
        (let ((sha (mod-patch--sha1-file orig)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (mod-patch--insert-header sha)
              (write-region (point-min) (point-max) patch-file nil 'silent)))))
       ;; anything else → diff failure
       (t
        (error "diff failed with code %s" status))))
    (kill-buffer buf)))

(defun mod-patch--apply-patch (orig patch-file out)
  "Apply PATCH-FILE to ORIG, write result to OUT, return t if no rejects."
  ;; `patch -u -l --fuzz=3` : unified, ignore whitespace, allow context drift
  (let ((status (call-process "patch" nil nil nil "-u" "-l" "--fuzz=3"
                              "-o" out orig patch-file)))
    ;; status 0 = clean, 1 = applied with fuzz/rejects, >1 = fatal
    (and (<= status 1)
         (not (file-exists-p (concat out ".rej"))))))

(defun mod-patch--refresh-patch-for-change (orig patch-file)
  "Regenerate PATCH-FILE so it applies cleanly to current ORIG."
  (unless (file-exists-p patch-file)
    (message "No patch file yet; nothing to refresh") (cl-return-from mod-patch--refresh-patch-for-change))

  (let* ((orig-sha (mod-patch--sha1-file orig))
         (header-sha (with-temp-buffer
                       (insert-file-contents patch-file)
                       (mod-patch--extract-header-sha1))))
    (when (string= orig-sha header-sha)
      (cl-return-from mod-patch--refresh-patch-for-change)) ; up-to-date

    (let* ((patched (make-temp-file "mod-patch-new"))
           (ok      (mod-patch--apply-patch orig patch-file patched)))
      (if ok
          (mod-patch--write-diff orig patched patch-file)
        ;; Conflicts – open merge buffer
        (let ((merge-buf (find-file-noselect patched)))
          (with-current-buffer merge-buf
            (smerge-mode 1)
            (setq-local mod-patch--orig-file orig
                        mod-patch--patch-file patch-file))
          (display-buffer merge-buf)
          (user-error "Patch no longer applies – resolve conflicts, then run M-x mod-patch-merge-finish"))))))

(defun mod-patch-merge-finish ()
  "Finish an interactive merge buffer created by `mod-patch--refresh-patch-for-change'."
  (interactive)
  (unless (and (boundp 'mod-patch--orig-file) (boundp 'mod-patch--patch-file))
    (user-error "Not a merge buffer created by mod-patch"))
  (save-buffer)
  (mod-patch--write-diff mod-patch--orig-file (buffer-file-name) mod-patch--patch-file)
  (kill-buffer (current-buffer))
  (message "Patch refreshed."))

;;;; ---------------------------------------------------------------------
;;;; Minor mode that *redirects* saves
;;;; ---------------------------------------------------------------------

(defun mod-patch--save-as-patch ()
  "Intercept `write-file-functions' in `mod-patch-mode'."
  (let* ((orig  (buffer-file-name))
         (tmp   (make-temp-file "mod-patch-edit"))
         (patch (mod-patch--patch-file orig)))
    (write-region (point-min) (point-max) tmp nil 'silent)
    (mod-patch--write-diff orig tmp patch)
    (delete-file tmp)
    (mod-patch--recent-push orig patch)
    (set-buffer-modified-p nil)
    t)) ; signal that save was handled

;;;###autoload
(define-minor-mode mod-patch-mode
  "Edit a virtual file; saving produces/updates a .patch."
  :lighter " Patch"
  (if mod-patch-mode
      (add-hook 'write-file-functions #'mod-patch--save-as-patch nil t)
    (remove-hook 'write-file-functions #'mod-patch--save-as-patch t)))

;;;; ---------------------------------------------------------------------
;;;; Minor mode for editing the *real* file
;;;; ---------------------------------------------------------------------

;;;###autoload
(define-minor-mode mod-patch-original-mode
  "Edit the real file; keep patch in sync on every save."
  :lighter " Orig"
  (if mod-patch-original-mode
      (add-hook 'after-save-hook #'mod-patch--after-real-save nil t)
    (remove-hook 'after-save-hook #'mod-patch--after-real-save t)))

;;;; ---------------------------------------------------------------------
;;;; Recent targets ring and switching helpers
;;;; ---------------------------------------------------------------------

(defvar mod-patch--recent nil
  "Alist (ORIGINAL . PATCH) with most recent first.")

(defun mod-patch--recent-push (orig patch)
  "Record (ORIG . PATCH) pair at head of `mod-patch--recent'."
  (setq mod-patch--recent
        (cons (cons orig patch)
              (cl-remove-if (lambda (c) (equal (car c) orig))
                            mod-patch--recent)))
  (when (> (length mod-patch--recent) mod-patch-recent-limit)
    (nbutlast mod-patch--recent (- (length mod-patch--recent)
                                   mod-patch-recent-limit))))

;;;###autoload
(defun mod-patch-switch-target ()
  "Switch to another remembered ORIGINAL file and enable suitable mode."
  (interactive)
  (unless mod-patch--recent
    (user-error "No recent mod-patch targets"))
  (let* ((orig (completing-read "Original: " mod-patch--recent nil t))
         (patch (cdr (assoc orig mod-patch--recent))))
    (find-file orig)
    (setq-local mod-patch-target-directory
                (file-name-directory patch))
    (mod-patch-original-mode 1)))

(defun mod-patch--show-patched-content ()
  "Replace current buffer with ORIGINAL ⊕ PATCH so you see the virtual file.
Assumes `mod-patch-target-directory' is already set."
  (let* ((orig   (buffer-file-name))
         (patch  (mod-patch--patch-file orig))
         (point  (point)))
    (if (and patch (file-exists-p patch))
        (let ((patched (make-temp-file "mod-patch-view")))
          (unwind-protect
              (if (mod-patch--apply-patch orig patch patched)
                  (progn
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert-file-contents patched)
                      (goto-char (min point (point-max)))
                      (set-buffer-modified-p nil)))
                (message "Patch failed to apply cleanly – showing original."))
            (delete-file patched)))
      (message "No patch file found; showing original."))))

;;;###autoload
(defun mod-patch-toggle ()
  "Toggle between editing the real file and the virtual patched view."
  (interactive)
  ;; Always save first so nothing is lost.
  (save-buffer)
  (cond
   ;; --- Currently in patch view → switch to original -------------------
   (mod-patch-mode
    (mod-patch-mode -1)
    (mod-patch-original-mode 1)
    (revert-buffer :ignore-auto :noconfirm))
   ;; --- Currently in original view → switch to patch view --------------
   (mod-patch-original-mode
    (mod-patch-original-mode -1)
    (mod-patch-mode 1)
    (mod-patch--show-patched-content))
   ;; --- Neither mode active: default to patch view ---------------------
   (t
    (mod-patch-mode 1)
    (mod-patch--show-patched-content))))

;; ---------------------------------------------------------------------
;;  BEGIN multi-directory patch support
;;  (copy the whole block verbatim)
;; ---------------------------------------------------------------------

;;;; ---------------------------------------------------------------------
;;;; 1.  Persistent index  (original-file  .  (dir1 dir2 …))
;;;; ---------------------------------------------------------------------

(defgroup mod-patch nil "Live patch authoring" :group 'tools)

(defcustom mod-patch-index-file
  (expand-file-name "mod-patch-index.el" user-emacs-directory)
  "File that stores the mapping from originals to patch directories."
  :type 'file :group 'mod-patch)

(defvar mod-patch--file->dirs nil
  "Alist where car is canonical original path, cdr is list of directories.")

;; Load persisted mapping, ignoring errors if the file is absent / malformed.
(load mod-patch-index-file t t)

(defun mod-patch--save-index ()
  "Write `mod-patch--file->dirs' to `mod-patch-index-file'."
  (with-temp-file mod-patch-index-file
    (insert ";; Auto-generated by mod-patch.el – DO NOT EDIT BY HAND.\n\n")
    (prin1 `(setq mod-patch--file->dirs ',mod-patch--file->dirs) (current-buffer))
    (insert "\n")))

(defun mod-patch--canonical (file)
  "Return canonicalised absolute path for FILE."
  (file-truename (expand-file-name file)))

(defun mod-patch--add-dir-for-orig (orig dir)
  "Record that ORIG uses DIR for patches, persist change."
  (setq orig (mod-patch--canonical orig)
        dir  (file-name-as-directory (expand-file-name dir)))
  (let ((cell (assoc orig mod-patch--file->dirs)))
    (if cell
        (setcdr cell (cl-remove-duplicates (cons dir (cdr cell))
                                           :test #'string-equal))
      (push (cons orig (list dir)) mod-patch--file->dirs)))
  (mod-patch--save-index))

(defun mod-patch--dirs-for-orig (orig)
  "Return list of patch directories registered for ORIG (may be nil)."
  (cdr (assoc (mod-patch--canonical orig) mod-patch--file->dirs)))

;;;; ---------------------------------------------------------------------
;;;; 2.  Utility: build patch file name for *any* directory
;;;; ---------------------------------------------------------------------

(defun mod-patch--patch-file (orig &optional dir)
  "Return the .patch path for ORIG in DIR.
If DIR is nil fall back to the buffer-local `mod-patch-target-directory'
or the directory of ORIG."
  (let ((d (or dir mod-patch-target-directory
               (file-name-directory orig))))
    (expand-file-name (concat (file-name-nondirectory orig) ".patch")
                      (file-name-as-directory d))))

;;;; ---------------------------------------------------------------------
;;;; 3.  Update `mod-patch-setup-for-current-buffer`
;;;; ---------------------------------------------------------------------

;;;###autoload
(defun mod-patch-setup-for-current-buffer (patch-dir)
  "Turn on `mod-patch-mode' and register PATCH-DIR for this original file."
  (interactive "DWrite patch in directory: ")
  (let ((orig (or (buffer-file-name)
                  (user-error "Buffer is not visiting a file"))))
    (setq-local mod-patch-target-directory
                (file-name-as-directory (expand-file-name patch-dir)))
    (mod-patch--add-dir-for-orig orig mod-patch-target-directory)
    (mod-patch-mode 1)
    (mod-patch--show-patched-content)))

;;;; ---------------------------------------------------------------------
;;;; 4.  Refresh *all* patches after saving the real file
;;;; ---------------------------------------------------------------------

(defun mod-patch--after-real-save ()
  "Regenerate every registered patch for this original."
  (let* ((orig (buffer-file-name))
         (dirs (mod-patch--dirs-for-orig orig)))
    (dolist (dir dirs)
      (let ((patch (mod-patch--patch-file orig dir)))
        (mod-patch--refresh-patch-for-change orig patch)
        (mod-patch--recent-push orig patch)))))

;;;; ---------------------------------------------------------------------
;;;; 5.  Keep compatibility for patch-buffers (no change needed)
;;;; ---------------------------------------------------------------------
;;    `mod-patch-mode' (the virtual-file mode) continues to use the
;;    buffer-local `mod-patch-target-directory` exactly as before, so no
;;    other part of the library needs an edit.

;; ---------------------------------------------------------------------
;;  END multi-directory patch support
;; ---------------------------------------------------------------------

(provide 'mod-patch)
;;; mod-patch.el ends here
