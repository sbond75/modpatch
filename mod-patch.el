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

(defun mod-patch--apply-patch (orig patch-file out &optional fuzz)
  "Apply PATCH-FILE (a unified diff) to ORIG, writing the merged result to OUT.
We call GNU patch in --merge mode, so even if hunks don't match exactly
they appear in OUT with conflict markers instead of being dropped.
Return non-nil if the application looked clean enough (no obvious rejects).
Optional FUZZ (integer) controls --fuzz=N tolerance."
  (let* ((fuzz (or fuzz 3))
         ;; We run patch with:
         ;;   -u: unified diff
         ;;   -l: ignore whitespace
         ;;   --merge: inline conflict markers instead of .rej files
         ;;   --fuzz=N: allow context drift
         ;;   -o OUT: write merged result to OUT
         (status (call-process
                  "patch" nil nil nil
                  "-u"
                  "-l"
                  (format "--fuzz=%d" fuzz)
                  "--merge"
                  "-o" out
                  orig patch-file)))
    ;; GNU patch exit codes:
    ;;   0 = success, no conflicts
    ;;   1 = applied with fuzz / offsets / possibly conflicts
    ;;   2+ = unrecoverable
    ;;
    ;; With --merge, even conflicts are written inline to OUT.
    ;; We treat 0 and 1 as usable results, because OUT always exists
    ;; and contains either clean merged content or conflict markers.
    (<= status 1)))

(defun mod-patch--refresh-patch-for-change (orig patch-file)
  "Rebase PATCH-FILE onto the current contents of ORIG.

If the patch's recorded base hash matches ORIG's current hash,
there's nothing to do.

Otherwise:
  1. Try to reapply the patch (with high fuzz and --merge) to produce
     a merged result representing \"your mod on top of new ORIG\".
  2. If that merged result has no conflict markers, auto-generate a
     new patch against ORIG and overwrite PATCH-FILE.
  3. If the merged result still has conflict markers, open it in a
     buffer for manual conflict resolution. After resolving, run
     `mod-patch-merge-finish` to produce the final patch.

This never silently destroys PATCH-FILE."
  (unless (file-exists-p patch-file)
    ;; Nothing to refresh because there's no patch file yet.
    (cl-return-from mod-patch--refresh-patch-for-change))

  ;; Compute the current upstream hash and compare to the patch's header hash.
  (let* ((current-sha (mod-patch--sha1-file orig))
         (stored-sha
          (with-temp-buffer
            (insert-file-contents patch-file)
            (goto-char (point-min))
            (when (looking-at "^---- MOD-PATCH base-sha1:\\([0-9a-f]+\\) ----")
              (match-string 1))))
         ;; Temporary file that will hold the patch applied to the *new* orig.
         (merged-tmp (make-temp-file "mod-patch-merged")))
    ;; If the patch is already based on this exact upstream content, skip.
    (when (and stored-sha (string= stored-sha current-sha))
      (cl-return-from mod-patch--refresh-patch-for-change))

    ;; Try to apply the existing patch to the new upstream with generous fuzz.
    (let* ((ok-high (mod-patch--apply-patch orig patch-file merged-tmp 10))
           (ok-low  (or ok-high
                        (mod-patch--apply-patch orig patch-file merged-tmp 3))))
      (if ok-low
          ;; We produced merged-tmp (which now represents: upstream+your changes,
          ;; possibly with conflict markers if there were true overlaps).
          (with-temp-buffer
            (insert-file-contents merged-tmp)
            (goto-char (point-min))
            (if (re-search-forward "^<<<<<<< " nil t)
                ;; We hit real conflicts. Hand control to the user.
                (let ((merge-buf (find-file-noselect merged-tmp)))
                  (with-current-buffer merge-buf
                    (when (fboundp 'smerge-mode)
                      (smerge-mode 1))
                    ;; Stash context so `mod-patch-merge-finish` knows what to diff.
                    (setq-local mod-patch--orig-file orig
                                mod-patch--patch-file patch-file
                                mod-patch--patched-tmp merged-tmp))
                  (display-buffer merge-buf)
                  (user-error
                   "Upstream changed nearby code. Resolve conflicts, then M-x mod-patch-merge-finish"))
              ;; No conflict markers. We can safely regenerate PATCH-FILE now.
              (mod-patch--write-diff orig merged-tmp patch-file)
              (delete-file merged-tmp)))
        ;; Severe failure (status ≥2). This should be rare with --merge,
        ;; but if it happens we still surface the merged attempt (which
        ;; may be partial) for manual salvage.
        (let ((merge-buf (find-file-noselect merged-tmp)))
          (with-current-buffer merge-buf
            (when (fboundp 'smerge-mode)
              (smerge-mode 1))
            (setq-local mod-patch--orig-file orig
                        mod-patch--patch-file patch-file
                        mod-patch--patched-tmp merged-tmp))
          (display-buffer merge-buf)
          (user-error
           "Patch could not be auto-rebased. Resolve manually, then M-x mod-patch-merge-finish"))))))

(defun mod-patch-merge-finish ()
  "Finalize manual conflict resolution begun by `mod-patch--refresh-patch-for-change'.

You should run this in the merge buffer that contains the resolved
combined result (your patch content rebased onto new upstream)."
  (interactive)
  (unless (and (boundp 'mod-patch--orig-file)
               (boundp 'mod-patch--patch-file)
               (boundp 'mod-patch--patched-tmp)
               mod-patch--orig-file
               mod-patch--patch-file
               mod-patch--patched-tmp)
    (user-error "This buffer is not an active mod-patch merge buffer"))

  ;; Ensure user's latest resolution is on disk
  (save-buffer)

  ;; Write / refresh the .patch using keep-empty semantics
  (mod-patch--write-diff-keep-empty
   mod-patch--orig-file
   (buffer-file-name)
   mod-patch--patch-file)

  ;; Clean up temp file and merge buffer
  (let ((tmpfile mod-patch--patched-tmp))
    (kill-buffer (current-buffer))
    (when (and tmpfile (file-exists-p tmpfile))
      (delete-file tmpfile)))

  (message "Patch rebased and updated."))

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
  :lighter (:eval (mod-patch--lighter-patch))
  (if mod-patch-mode
      (add-hook 'write-file-functions #'mod-patch--save-as-patch nil t)
    (remove-hook 'write-file-functions #'mod-patch--save-as-patch t)))

;;;; ---------------------------------------------------------------------
;;;; Minor mode for editing the *real* file
;;;; ---------------------------------------------------------------------

;;;###autoload
(define-minor-mode mod-patch-original-mode
  "Edit the real file; keep patch in sync on every save."
  :lighter (:eval (mod-patch--lighter-orig))
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

;;;###autoload
(defun mod-patch-toggle ()
  "Toggle between editing the real file and the virtual patched view."
  (interactive)
  (save-buffer)                               ; flush edits first
  (cond
   ;; From Patch → Orig
   (mod-patch-mode
    (mod-patch-mode -1)                       ; !! turn off patch view
    (mod-patch-original-mode 1)
    (revert-buffer :ignore-auto :noconfirm))
   ;; From Orig → Patch
   (mod-patch-original-mode
    (mod-patch-original-mode -1)
    (mod-patch-mode 1)
    (mod-patch--show-patched-content)
    (mod-patch--refresh-font-lock))
   ;; Neither mode yet – default to Patch
   (t
    (mod-patch-mode 1)
    (mod-patch--show-patched-content)
    (mod-patch--refresh-font-lock))))

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

;;;; ---------------------------------------------------------------------
;;;;  Persistent *session* state  (original → plist)
;;;; ---------------------------------------------------------------------

(defcustom mod-patch-state-file
  (expand-file-name "mod-patch-state.el" user-emacs-directory)
  "File used to remember the last active mode and patch directory
for every original file."
  :type 'file :group 'mod-patch)

(defvar mod-patch--file-state nil
  "Alist (ORIG . (:last-mode symbol  :last-dir string)).")

(load mod-patch-state-file t t)   ; silently ignore if missing

(defun mod-patch--save-state ()
  (with-temp-file mod-patch-state-file
    (insert ";; Auto-generated by mod-patch – DO NOT EDIT MANUALLY.\n\n")
    (prin1 `(setq mod-patch--file-state ',mod-patch--file-state) (current-buffer))
    (insert "\n")))

(defun mod-patch--record-session (orig mode dir)
  "Remember that ORIG last used MODE ('patch or 'orig) with DIR."
  (setq orig (mod-patch--canonical orig)
        dir  (file-name-as-directory (expand-file-name dir)))
  (setf (alist-get orig mod-patch--file-state nil nil #'string-equal)
        (list :last-mode mode :last-dir dir))
  (mod-patch--save-state))

(advice-add 'mod-patch-mode
            :after
            (lambda (&rest _)
              (when (and mod-patch-mode (buffer-file-name))
                (mod-patch--record-session
                 (buffer-file-name) 'patch mod-patch-target-directory))))

(advice-add 'mod-patch-original-mode
            :after
            (lambda (&rest _)
              (when (and mod-patch-original-mode (buffer-file-name))
                (let* ((orig  (mod-patch--canonical (buffer-file-name)))
                       (state (alist-get orig mod-patch--file-state
                                         nil nil #'string-equal))
                       (dir   (plist-get state :last-dir))) ; keep previous dir
                  (mod-patch--record-session orig 'orig dir)))))

(defun mod-patch--maybe-auto-restore ()
  "If current buffer is an original asset seen before, restore last mode."
  (let* ((orig (buffer-file-name))
         (state (and orig
                     (alist-get (mod-patch--canonical orig)
                                mod-patch--file-state nil nil #'string-equal))))
    (when state
      (pcase-let ((`(:last-mode ,last :last-dir ,dir) state))
        (setq-local mod-patch-target-directory dir)
        (cond
         ((eq last 'patch)
          ;; ensure directory is still registered
          (mod-patch--add-dir-for-orig orig dir)
          (mod-patch-mode 1)
          (mod-patch--show-patched-content))
         ((eq last 'orig)
          (mod-patch-original-mode 1)))))))

(add-hook 'find-file-hook #'mod-patch--maybe-auto-restore)

;;;; ---------------------------------------------------------------------
;;;; Switch the current buffer to a different patch directory
;;;; ---------------------------------------------------------------------

(defun mod-patch--completing-patch-dirs (orig)
  "Return an alist suitable for `completing-read' that lists every
registered patch directory for ORIG.  Recent-save order first,
followed by the remaining dirs, duplicates removed."
  (let* ((canon (mod-patch--canonical orig))
         (recent (mapcar #'cdr
                         (cl-remove-if-not
                          (lambda (pair) (string-equal (car pair) canon))
                          mod-patch--recent)))
         (all    (mod-patch--dirs-for-orig canon))
         (seen   nil)                      ; <- local variable
         res)
    ;; Build result list
    (dolist (d (append recent all))
      (unless (member d seen)
        (push d res)
        (push d seen)))
    (nreverse res)))

;;;###autoload
(defun mod-patch-switch-patch ()
  "Interactively choose one of the patch directories for the current file
and enter Patch-mode, showing that patched view."
  (interactive)
  (let ((orig (or (buffer-file-name)
                  (user-error "Buffer is not visiting a file"))))
    (let* ((choices (mod-patch--completing-patch-dirs orig))
           (dir (completing-read "Patch directory: " choices nil t)))
      (unless dir (user-error "No patch directory chosen"))
      ;; Save any edits before switching views
      (save-buffer)
      ;; Register and activate chosen directory
      (setq-local mod-patch-target-directory
                  (file-name-as-directory dir))
      (mod-patch--add-dir-for-orig orig dir)
      ;; Ensure we are in Patch-mode, not Orig-mode
      (when mod-patch-original-mode
        (mod-patch-original-mode -1))
      (mod-patch-mode 1)
      (mod-patch--show-patched-content)
      (message "Now editing virtual file backed by: %s" dir))))

;;;; ---------------------------------------------------------------------
;;;; Modeline helpers
;;;; ---------------------------------------------------------------------

(defun mod-patch--lighter-orig ()
  "Return the modeline string for `mod-patch-original-mode'."
  " Orig")

;;;; ---------------------------------------------------------------------
;;;;  FIX 1  –  buffer replacement that keeps undo history *and* silences
;;;;             undo-fu-session
;;;; ---------------------------------------------------------------------

(defun mod-patch--show-patched-content ()
  "Replace current buffer text with ORIGINAL ⊕ PATCH.

Normal case:
  - We apply the patch with generous fuzz and inline merge support.
  - We show that merged result in the buffer.
  - Undo history is not polluted.
  - Syntax highlighting and modeline are refreshed.

Hard failure case (patch exit status >=2):
  - We fall back to the pristine original contents in this buffer.
  - We open a separate buffer *mod-patch-failure* that includes:
        - the failing status code
        - the patch file path
        - the contents of the patch file
    so you can inspect what went wrong.

Either way, we do not silently leave you in an inconsistent half-applied state."
  (let* ((orig   (buffer-file-name))
         (patch  (mod-patch--patch-file orig))
         (pt     (point))
         (undo-fu-active (and (boundp 'undo-fu-session-mode)
                              undo-fu-session-mode)))
    (unless (and orig (file-readable-p orig))
      (user-error "This buffer is not visiting a readable file on disk"))

    ;; Temporarily disable undo-fu session persistence so we don't corrupt its
    ;; saved file lengths with wholesale buffer replacement.
    (when undo-fu-active
      (undo-fu-session-mode -1))

    (unwind-protect
        (if (and patch (file-readable-p patch))
            ;; Try to apply the patch to the current original.
            (let ((merged-tmp (make-temp-file "mod-patch-view-")))
              (let ((ok-or-status (mod-patch--try-apply-patch-for-view
                                   orig patch merged-tmp)))
                (if (eq ok-or-status t)
                    ;; Successful apply (or at worst conflict markers inline).
                    (let ((src (generate-new-buffer " *mod-patch-src*")))
                      (unwind-protect
                          (progn
                            ;; Load merged text into a temp buffer
                            (with-current-buffer src
                              (insert-file-contents merged-tmp))
                            ;; Replace this buffer with merged text, but
                            ;; don't record this gigantic edit in undo.
                            (let ((buffer-undo-list t))
                              (replace-buffer-contents src)))
                        (kill-buffer src)))
                  ;; Hard failure (status >= 2). We *don't* leave a half-applied
                  ;; buffer here. Instead: show the pristine original in this
                  ;; buffer, and create a diagnostic buffer so the user can see
                  ;; what failed and inspect the patch hunks manually.
                  (let ((src (generate-new-buffer " *mod-original-src*")))
                    (unwind-protect
                        (progn
                          ;; Insert the pristine original.
                          (with-current-buffer src
                            (insert-file-contents orig))
                          (let ((buffer-undo-list t))
                            (replace-buffer-contents src)))
                      (kill-buffer src)))
                  ;; Now open a diagnostic report so the user can investigate.
                  (let ((report (get-buffer-create "*mod-patch-failure*")))
                    (with-current-buffer report
                      (read-only-mode -1)
                      (erase-buffer)
                      (insert (format
                               "Patch failed to apply to %s\nExit status: %s\nPatch file: %s\n\n--- PATCH CONTENTS BELOW ---\n\n"
                               orig ok-or-status patch))
                      (if (file-readable-p patch)
                          (insert-file-contents patch)
                        (insert "[Patch file not readable]\n"))
                      (goto-char (point-min))
                      (read-only-mode 1))
                    (display-buffer report)))
              ;; cleanup temp file
              ;; (don't leave merged-tmp lingering unnecessarily)
              ;; Only delete if it exists and wasn't repurposed elsewhere:
              (ignore-errors (delete-file merged-tmp)))
          ;; No patch file is readable at all. Just show original.
          (let ((src (generate-new-buffer " *mod-original-src*")))
            (unwind-protect
                (progn
                  (with-current-buffer src
                    (insert-file-contents orig))
                  (let ((buffer-undo-list t))
                    (replace-buffer-contents src)))
              (kill-buffer src)))))

      ;; Re-enable undo-fu session persistence if it was active.
      (when (and (not undo-fu-session-mode) undo-fu-active)
        (undo-fu-session-mode 1)))

    ;; Restore point to roughly where it was before swapping text.
    (goto-char (min pt (point-max)))
    ;; Mark buffer clean.
    (set-buffer-modified-p nil)

    ;; Finally, refresh syntax highlighting and mode line.
    (mod-patch--refresh-display)))

(defun mod-patch--show-original-content ()
  "Replace current buffer text with the pristine on-disk original.
Does not poison undo history, and refreshes highlighting/modeline."
  (let* ((orig   (buffer-file-name))
         (pt     (point))
         (undo-fu-active (and (boundp 'undo-fu-session-mode)
                              undo-fu-session-mode)))
    (unless (and orig (file-readable-p orig))
      (user-error "Cannot load original contents; no readable file for this buffer"))
    ;; Temporarily disable undo-fu session persistence.
    (when undo-fu-active (undo-fu-session-mode -1))
    (unwind-protect
        (let ((src (generate-new-buffer " *mod-original-src*")))
          (unwind-protect
              (progn
                (with-current-buffer src
                  (insert-file-contents orig))
                ;; Don't record in undo, but DO run modification hooks.
                (let ((buffer-undo-list t))
                  (replace-buffer-contents src)))
            (kill-buffer src)))
      ;; Re-enable undo-fu session if it was active.
      (when (and (not undo-fu-session-mode) undo-fu-active)
        (undo-fu-session-mode 1)))
    ;; Restore point and mark buffer clean.
    (goto-char (min pt (point-max)))
    (set-buffer-modified-p nil)
    ;; Repaint highlighting + mode line.
    (mod-patch--refresh-display)))

;; Ensure undo-fu is disabled every time Patch-mode turns on
(advice-add 'mod-patch-mode :after
            (lambda (&rest _)
              (when mod-patch-mode
                (when (and (boundp 'undo-fu-session-mode)
                           undo-fu-session-mode)
                  (undo-fu-session-mode -1)))
              (when (not mod-patch-mode)
                (when (and (boundp 'undo-fu-session-mode)
                           (not undo-fu-session-mode))
                  (undo-fu-session-mode 1)))))

;;;; ---------------------------------------------------------------------
;;;;  Disambiguating lighter – robust, no cl-return
;;;; ---------------------------------------------------------------------

(defun mod-patch--unique-suffix (dir dirlist)
  "Return the shortest trailing path of DIR that is unique in DIRLIST.
All paths are canonical absolute directories."
  (setq dir (directory-file-name dir)
        dirlist (mapcar #'directory-file-name dirlist))
  (let* ((parts (split-string dir "/" t))          ; forward order
         (len   (length parts)))
    (cl-loop for n from 1 to len                  ; 1 = basename only
             for suffix = (string-join (nthcdr (- len n) parts) "/")
             unless (cl-some
                     (lambda (other)
                       (and (not (file-equal-p other dir))
                            (string-suffix-p suffix other)))
                     dirlist)
             return suffix
             finally return (car (last parts))))) ; fallback = basename

(defun mod-patch--lighter-patch ()
  "Modeline text for `mod-patch-mode', showing unambiguous patch directory."
  (let* ((orig   (buffer-file-name))
         (target mod-patch-target-directory))
    (if (and orig target (file-directory-p target))
        (let* ((dirs   (mapcar #'mod-patch--canonical
                               (mod-patch--dirs-for-orig orig)))
               (suffix (mod-patch--unique-suffix
                        (mod-patch--canonical target) dirs)))
          (format " Patch[%s]" suffix))
      " Patch")))

;;;; ---------------------------------------------------------------------
;;;;  Revert-buffer advice: keep directory *and* restore patched content
;;;; ---------------------------------------------------------------------

(defun mod-patch--revert-keep-patch-dir (orig-fn &rest args)
  "Around advice for `revert-buffer'.
If the buffer was in Patch-view, restore the virtual contents after revert
and preserve `mod-patch-target-directory'."
  (let ((saved-dir mod-patch-target-directory)
        (was-patch mod-patch-mode))
    ;; run the real revert
    (prog1 (apply orig-fn args)
      ;; post-revert actions
      (setq-local mod-patch-target-directory saved-dir)
      (when was-patch                  ; still in Patch-mode?
        (mod-patch--show-patched-content)))))

(advice-add 'revert-buffer :around #'mod-patch--revert-keep-patch-dir)

;;;; Etc.

(defun mod-patch--refresh-font-lock ()
  "Re-run fontification, including Tree-sitter overlays if available."
  ;; Flush classic font-lock
  (font-lock-flush)
  (font-lock-ensure)
  ;; Tree-sitter (Emacs 29+).  These symbols are present only when
  ;; the built-in TS integration is loaded, so wrap in `fboundp'.
  (when (and (fboundp 'treesit-major-mode-auto-configure)
             (bound-and-true-p treesit-font-lock-enabled))
    ;; Recompute feature mapping and force a pass over the whole buffer.
    (treesit-font-lock-recompute-features)
    (treesit-font-lock-recompute-features 'force)))

;;;; ---------------------------------------------------------------------
;;;; 1.  Canonical helpers
;;;; ---------------------------------------------------------------------

(defun mod-patch--canonical (file) (file-truename (expand-file-name file)))

(defun mod-patch--current-dir ()
  "Return buffer-local patch dir (string) or nil."
  (and (boundp 'mod-patch-target-directory) mod-patch-target-directory))

(defun mod-patch--current-mode-symbol ()
  (cond (mod-patch-mode           'patch)
        (mod-patch-original-mode  'orig)
        (t                        nil)))

;;;; ---------------------------------------------------------------------
;;;; 2.  Robust session recording triggered on *every* save or buffer kill
;;;; ---------------------------------------------------------------------

(defun mod-patch--record-session (&rest _)
  "Persist current view + directory for this original file.
Ignores any arguments so it is safe on all hooks and advice."
  (when-let* ((orig (buffer-file-name))
              (mode (mod-patch--current-mode-symbol)))
    (let* ((orig (mod-patch--canonical orig))
           (dir  (or (mod-patch--current-dir)
                     (plist-get (alist-get orig mod-patch--file-state
                                           nil nil #'string-equal)
                                :last-dir)))
           (payload (list :last-mode mode :last-dir dir))
           (cell (cl-assoc orig mod-patch--file-state :test #'string-equal)))
      (if cell
          (setcdr cell payload)           ; update existing entry
        (push (cons orig payload) mod-patch--file-state)) ; new entry
      (mod-patch--save-state))))

(add-hook 'after-save-hook       #'mod-patch--record-session)
(add-hook 'kill-buffer-hook      #'mod-patch--record-session)

;;;; ---------------------------------------------------------------------
;;;; 3.  Auto-restore when reopening a file
;;;; ---------------------------------------------------------------------

(defun mod-patch--maybe-auto-restore ()
  "Restore view/dir captured in `mod-patch--file-state'."
  (when-let* ((orig (buffer-file-name))
              (state (alist-get (mod-patch--canonical orig)
                                mod-patch--file-state nil nil #'string-equal))
              (mode  (plist-get state :last-mode))
              (dir   (plist-get state :last-dir)))
    (setq-local mod-patch-target-directory dir)
    (mod-patch--add-dir-for-orig orig dir)     ; guarantee it's registered
    (pcase mode
      ('patch (mod-patch-mode 1)
              (mod-patch--show-patched-content))
      ('orig  (mod-patch-original-mode 1)))))

;; Move hook to the *front* so we run before other find-file logic.
(add-hook 'find-file-hook #'mod-patch--maybe-auto-restore 0)

;;;; ---------------------------------------------------------------------
;;;; 4.  Reliable view toggler
;;;; ---------------------------------------------------------------------

(defun mod-patch-toggle ()
  "Toggle between original file and the currently selected patch."
  (interactive)
  ;; If no view is active, restore last saved state or ask user to pick a patch.
  (unless (or mod-patch-mode mod-patch-original-mode)
    (if (mod-patch--current-dir)
        (mod-patch-mode 1)
      (user-error "No patch directory selected; run `mod-patch-switch-patch' or `mod-patch-setup-for-current-buffer' first")))
  ;; Save edits before changing the buffer contents.
  (save-buffer)
  (cond
   ;; ----- Patch → Orig ----------------------------------------------------
   (mod-patch-mode
    (mod-patch-mode -1)
    (mod-patch-original-mode 1)
    (revert-buffer :ignore-auto :noconfirm))
   ;; ----- Orig → Patch ----------------------------------------------------
   (mod-patch-original-mode
    (mod-patch-original-mode -1)
    (mod-patch-mode 1)
    (mod-patch--show-patched-content))
   ;; (we never reach here: the guard above ensures one mode is active)
   ))

;;;; ---------------------------------------------------------------------
;;;; 1.  Unified display refresh (modeline + all font-lock back-ends)
;;;; ---------------------------------------------------------------------

(defun mod-patch--refresh-display ()
  "Repaint font-lock (classic and Tree-sitter) and update mode-line."
  ;; Classic font-lock.
  (font-lock-flush) (font-lock-ensure)

  ;; Built-in Tree-sitter (Emacs 29+).
  (when (and (featurep 'treesit) (bound-and-true-p treesit-font-lock-enabled))
    ;; Recompute features then refontify.
    (treesit-font-lock-recompute-features 'force))

  ;; External tree-sitter.el (≤ 28).
  (when (fboundp 'tree-sitter-hl-update)
    (tree-sitter-hl-update))

  ;; Finally, force global mode-line refresh.
  (force-mode-line-update t))

;;;; ---------------------------------------------------------------------
;;;; 2.  Robust view toggler
;;;; ---------------------------------------------------------------------

(defun mod-patch-toggle ()
  "Toggle between editing the pristine original file and the patched virtual view.
This does NOT call `revert-buffer`, so it preserves our modes
and guarantees the modeline lighter and syntax highlighting refresh."
  (interactive)

  ;; If neither mode is active yet (fresh visit), choose a view:
  ;; prefer Patch if we know a patch dir; otherwise Orig.
  (unless (or mod-patch-mode mod-patch-original-mode)
    (if (mod-patch--current-dir)
        (mod-patch-mode 1)
      (mod-patch-original-mode 1)))

  ;; Always save before switching, so patches / state stay in sync.
  (save-buffer)

  (cond
   ;; Currently looking at the patch view -> switch to original view.
   (mod-patch-mode
    ;; Turn off patch mode, turn on original mode.
    (mod-patch-mode -1)
    (mod-patch-original-mode 1)

    ;; Load pristine original file text into this buffer.
    (mod-patch--show-original-content)
    ;; At this point:
    ;;   mod-patch-original-mode == t
    ;;   mod-patch-mode == nil
    ;;   lighter should become Orig
    ;;   syntax highlighting already refreshed by show-original-content
    )

   ;; Currently looking at the original view -> switch to patch view.
   (mod-patch-original-mode
    ;; Turn off original mode, turn on patch mode.
    (mod-patch-original-mode -1)
    (mod-patch-mode 1)

    ;; Load patched text into this buffer.
    ;; This also refreshes highlighting and forces mode-line update.
    (mod-patch--show-patched-content)))

  ;; As an extra safety: global redisplay after mode flip.
  (force-mode-line-update 'all))

;;;; ---------------------------------------------------------------------
;;;; 3.  Revert advice – keep patch view *and* refresh
;;;; ---------------------------------------------------------------------

;; Remove any old advice, then add this:
(ignore-errors (advice-remove 'revert-buffer #'mod-patch--revert-keep-patch-dir))

(defun mod-patch--revert-keep-patch-dir (orig-fn &rest args)
  "After reverting, if we were in Patch view, restore patched content and refresh."
  (let ((saved-dir mod-patch-target-directory)
        (was-patch mod-patch-mode)
        (was-orig  mod-patch-original-mode))
    (prog1 (apply orig-fn args)
      ;; revert-buffer resets buffer-local vars, so re-establish our state
      (setq-local mod-patch-target-directory saved-dir)
      (cond
       (was-patch
        ;; We want to stay in patch mode, so re-enable it and load patched text.
        (mod-patch-mode 1)
        (mod-patch-original-mode -1)
        (mod-patch--show-patched-content))
       (was-orig
        ;; Stay in original mode.
        (mod-patch-original-mode 1)
        (mod-patch-mode -1)
        (mod-patch--show-original-content))
       (t
        ;; Neither was active; leave as-is.
        ))
      ;; Force repaint now that modes are restored.
      (force-mode-line-update 'all))))

(advice-add 'revert-buffer :around #'mod-patch--revert-keep-patch-dir)

(defun mod-patch--write-diff-keep-empty (orig tmp patch-file)
  "Write a patch for TMP vs ORIG into PATCH-FILE, always producing a file.
This is like `mod-patch--write-diff' except it will NOT delete PATCH-FILE
even if there are no differences left.
We still add the MOD-PATCH header with the new base sha1."
  (let ((buf (get-buffer-create "*mod-patch-diff*")))
    (with-current-buffer buf
      (erase-buffer))
    ;; -U12: generous context. -N: treat missing files as empty.
    (let ((status (call-process "diff" nil buf nil "-u" "-U12" "-N" orig tmp)))
      (cond
       ;; status 0 = no diff. We still create a header-only patch file.
       ((= status 0)
        (with-current-buffer buf
          (let ((sha (mod-patch--sha1-file orig)))
            (goto-char (point-min))
            (insert (format "---- MOD-PATCH base-sha1:%s ----\n" sha))
            (write-region (point-min) (point-max) patch-file nil 'silent))))
       ;; status 1 = diff produced. Prepend header and write.
       ((= status 1)
        (with-current-buffer buf
          (let ((sha (mod-patch--sha1-file orig)))
            (goto-char (point-min))
            (insert (format "---- MOD-PATCH base-sha1:%s ----\n" sha))
            (write-region (point-min) (point-max) patch-file nil 'silent))))
       (t
        (error "diff failed with exit code %s" status))))
    (kill-buffer buf)))

(defun mod-patch--try-apply-patch-for-view (orig patch-file out)
  "Attempt to apply PATCH-FILE to ORIG and write the merged result to OUT.
We use a tolerant invocation of GNU patch: unified (-u), whitespace-insensitive
(-l), --merge (inline conflicts instead of .rej), and high fuzz.

Return:
  t         if patch produced usable merged output (status 0 or 1)
  integer   if patch exited with >= 2, meaning it really failed.

This is meant for *viewing* a patched buffer, not for final rebase."
  (let* ((status (call-process
                  "patch" nil nil nil
                  "-u"
                  "-l"
                  "--merge"
                  "--fuzz=10"
                  "-o" out
                  orig patch-file)))
    (if (<= status 1)
        t
      status)))

;; ---------------------------------------------------------------------
;;  END multi-directory patch support
;; ---------------------------------------------------------------------

(provide 'mod-patch)
;;; mod-patch.el ends here
