;;; mod-patch.el --- Author Lua-style patch files; made by ChatGPT o3  -*- lexical-binding:t -*-

(defvar-local mod-patch-target-directory nil
  "Directory where the .patch file is written.
If nil, defaults to the directory of the original file.")

(defun mod-patch--patch-file-name (file)
  (let* ((dir (or mod-patch-target-directory
                  (file-name-directory file)))
         (base (file-name-nondirectory file)))
    (expand-file-name (concat base ".patch") dir)))

(defun mod-patch--buffer-to-temp ()
  "Write current buffer to a temp file, return the name."
  (let ((tmp (make-temp-file "mod-patch")))
    (write-region (point-min) (point-max) tmp nil 'silent)
    tmp))

(defun mod-patch--write-diff (a b patch-file)
  "Generate unified diff between files A and B into PATCH-FILE."
  (with-temp-buffer
    (let ((status (call-process "diff" nil t nil "-u" a b)))
      (cond
       ((= status 0)                 ; no differences
        (when (file-exists-p patch-file) (delete-file patch-file)))
       ((= status 1)                 ; diff produced
        (write-region (point-min) (point-max) patch-file nil 'silent))
       (t (error "diff failed with exit code %s" status))))))

(defun mod-patch--save-as-patch ()
  "Intercept `save-buffer' while `mod-patch-mode' is active."
  (let* ((orig (buffer-file-name))
         (tmp  (mod-patch--buffer-to-temp))
         (patch (mod-patch--patch-file-name orig)))
    (unwind-protect
        (mod-patch--write-diff orig tmp patch)
      (delete-file tmp))
    (set-buffer-modified-p nil)
    t))                              ; signal that we handled the save

;;;###autoload
(define-minor-mode mod-patch-mode
  "Edit buffer normally but save changes as a .patch file."
  :lighter " Patch"
  (if mod-patch-mode
      (add-hook 'write-file-functions #'mod-patch--save-as-patch nil t)
    (remove-hook 'write-file-functions #'mod-patch--save-as-patch t)))


;;;; Editing the *real* file and keeping the patch up to date ----------

(defun mod-patch--refresh-patch-after-real-save ()
  "After saving the real file, regenerate its patch."
  (let* ((orig (buffer-file-name))
         (patch (mod-patch--patch-file-name orig)))
    (when (file-exists-p patch)
      ;; Apply the old patch to the new original in a temp area
      (let* ((patched (make-temp-file "mod-patch-applied"))
             (status  (call-process "patch" nil nil nil "-o" patched orig patch)))
        (unless (= status 0)
          (error "Applying existing patch failed; patch not refreshed"))
        (mod-patch--write-diff orig patched patch)
        (delete-file patched)))))

;;;###autoload
(define-minor-mode mod-patch-original-mode
  "Edit the real file; saving also refreshes the corresponding .patch."
  :lighter " Orig"
  (if mod-patch-original-mode
      (add-hook 'after-save-hook #'mod-patch--refresh-patch-after-real-save nil t)
    (remove-hook 'after-save-hook #'mod-patch--refresh-patch-after-real-save t)))

;;;###autoload
(defun mod-patch-toggle ()
  "Switch between editing the patch proxy and the real file."
  (interactive)
  (if mod-patch-mode
      (progn
        (mod-patch-mode -1)
        (mod-patch-original-mode 1))
    (mod-patch-original-mode -1)
    (mod-patch-mode 1)))

(provide 'mod-patch)
