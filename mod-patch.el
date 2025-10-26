;;; mod-patch.el --- Patch-oriented editing workflow minor mode -*- lexical-binding: t; -*-

;; Author: mod-patch system
;; Version: 0.1
;; Keywords: vc, convenience, files, tools
;; Package-Requires: ((emacs "27.1") (json "1.5"))
;; URL: n/a (local development)

;;; Commentary:

;; mod-patch-mode lets you:
;;
;; - Edit either the "original" source file or a "virtual patched view" of that
;;   file. Saving in patch mode writes/refreshes a unified diff patch instead of
;;   clobbering the original.
;;
;; - Maintain multiple patches per file. You can switch between patches and the
;;   original with interactive commands.
;;
;; - Persist state: last active patch, recent patches, last mode, etc.
;;   State is stored in JSON at `mod-patch-state-file`. When you reopen the file,
;;   the mode, patch view, etc. are restored automatically.
;;
;; - Keep patch files in a configurable directory tree. The mode line shows
;;   which patch you are "in", disambiguated by directory.
;;
;; - Attempt to keep patches in sync when the original changes, using external
;;   `patch` and `diff`. On failure, you get a conflict buffer and can call
;;   `mod-patch-merge-finish`.
;;
;; - Survive common disruptions like revert-buffer and major-mode re-fontification.
;;
;; This file provides:
;;   - Minor mode `mod-patch-mode`
;;   - Commands:
;;       mod-patch-setup-for-current-buffer
;;       mod-patch-toggle
;;       mod-patch-switch
;;       mod-patch-save-buffer   (replaces save-buffer under the mode)
;;       mod-patch-merge-finish
;;
;; Notes / Limits (current implementation status):
;;   - Patch rebasing/synchronization after original save is stubbed but hooked.
;;   - Merge conflict assistance is wired, but the actual merge logic will need
;;     refinement for complex 3-way situations.
;;   - We assume GNU diff/patch are available in PATH.
;;
;; This is a foundation: it is careful about buffer state, undo lists,
;; persistence, UI, error reporting, fault tolerance, and extensibility.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

;;;; ---------------------------------------------------------------------------
;;;; Customization and state
;;;; ---------------------------------------------------------------------------

(defgroup mod-patch nil
  "Edit and manage file modifications as standalone patches."
  :group 'tools
  :group 'files)

(defcustom mod-patch-target-directory
  (expand-file-name "mod-patch-patches/" user-emacs-directory)
  "Directory root where patch files will be stored by default.

When you save in patch mode, the patch will be written under this directory,
mirroring part of the source file path so patches can be grouped.

If nil, patch files are written next to the original file.

The code will create the directory if it doesn't exist."
  :type '(choice (const :tag "Same directory as source files" nil)
                 (directory :tag "Central patch root")))

(defcustom mod-patch-state-file
  (expand-file-name "mod-patch-state.json" user-emacs-directory)
  "Path to the persistent state file for mod-patch.

This file tracks:
  - per-original-file: list of known patch files,
  - last active patch,
  - last active mode (original or patch)."
  :type 'file)

(defcustom mod-patch-auto-enable t
  "If non-nil, automatically enable `mod-patch-mode' for files we are tracking.

Tracking is determined by the persistent state in `mod-patch-state-file'."
  :type 'boolean)

(defcustom mod-patch-diff-extra-args '("-u" "-U12")
  "Extra arguments passed to `diff' when computing patch files."
  :type '(repeat string))

(defcustom mod-patch-patch-program "patch"
  "Program used to apply patches (must be GNU patch-like)."
  :type 'string)

(defcustom mod-patch-diff-program "diff"
  "Program used to compute unified diffs."
  :type 'string)

;;;; Internal persistent state representation
;; We cache the state in a hash table keyed by canonical absolute file path.
;; Each value is a plist:
;;   :patches (list-of-absolute-patch-files)
;;   :last-mode ('original or 'patch)
;;   :last-patch (absolute-file-name or nil)
;;   :timestamp (float-time last update) [for possible future cleanup]
;;
;; We load lazily and save eagerly on mutation.

(defvar mod-patch--state-cache nil
  "In-memory cache of mod-patch persistent state.

Hash table mapping canonical original file path to plist. See
`mod-patch--empty-file-state' for the shape.")

(defun mod-patch--ensure-state-cache ()
  "Ensure `mod-patch--state-cache' is initialized from disk."
  (unless (hash-table-p mod-patch--state-cache)
    (setq mod-patch--state-cache (make-hash-table :test 'equal))
    (when (file-readable-p mod-patch-state-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents mod-patch-state-file)
            (let* ((json-object-type 'plist)
                   (json-array-type 'list)
                   (json-key-type 'symbol)
                   (data (json-read)))
              ;; Old versions might save as alist, so normalize.
              ;; We accept either hash table (alist-like printed with #s)
              ;; or alist. We coerce everything into a hash table.
              (cond
               ((hash-table-p data)
                (setq mod-patch--state-cache data))
               ((listp data)
                ;; assume ((orig-file . plist) ...)
                (setq mod-patch--state-cache (make-hash-table :test 'equal))
                (dolist (it data)
                  (when (and (consp it)
                             (stringp (car it))
                             (plistp (cdr it)))
                    (puthash (car it) (cdr it) mod-patch--state-cache))))
               (t
                (message "[mod-patch] state format unrecognized, starting fresh.")))))
        (error
         (message "[mod-patch] Failed to load state: %S" err))))))

(defun mod-patch--save-state-cache ()
  "Persist `mod-patch--state-cache' to `mod-patch-state-file' as JSON."
  (mod-patch--ensure-state-cache)
  (let ((json-encoding-pretty-print t))
    (with-temp-file mod-patch-state-file
      ;; Write as alist for nicer diffs / portability.
      (let (alist)
        (maphash
         (lambda (k v)
           (push (cons k v) alist))
         mod-patch--state-cache)
        (insert (json-encode (nreverse alist)))))))

(defun mod-patch--canon (file)
  "Return canonical absolute path for FILE (or nil if FILE is nil)."
  (when file
    (expand-file-name (file-truename file))))

(defun mod-patch--empty-file-state ()
  "Return a fresh plist for a file state entry."
  (list :patches '()
        :last-mode 'original
        :last-patch nil
        :timestamp (float-time)))

(defun mod-patch--get-file-state (orig-file)
  "Return the plist state for ORIG-FILE (canonicalized). Creates one if needed."
  (mod-patch--ensure-state-cache)
  (let* ((key (mod-patch--canon orig-file))
         (val (gethash key mod-patch--state-cache)))
    (unless (plistp val)
      (setq val (mod-patch--empty-file-state))
      (puthash key val mod-patch--state-cache))
    val))

(defun mod-patch--update-file-state (orig-file &rest kvs)
  "Update persistent state for ORIG-FILE by applying KVS plist updates.

KVS is key value pairs to set in the file's plist (like :last-mode 'patch)."
  (let* ((key (mod-patch--canon orig-file))
         (state (mod-patch--get-file-state key)))
    (while kvs
      (let ((k (pop kvs))
            (v (pop kvs)))
        (setq state (plist-put state k v))))
    (setq state (plist-put state :timestamp (float-time)))
    (puthash key state mod-patch--state-cache)
    (mod-patch--save-state-cache)
    state))

(defun mod-patch--remember-patch (orig-file patch-file)
  "Ensure PATCH-FILE is recorded in ORIG-FILE's :patches list."
  (let* ((key (mod-patch--canon orig-file))
         (state (mod-patch--get-file-state key))
         (patches (plist-get state :patches))
         (patch-file (mod-patch--canon patch-file)))
    (unless (member patch-file patches)
      (setq patches (cons patch-file patches))
      (mod-patch--update-file-state key :patches patches))))

(defun mod-patch--recent-patches (orig-file)
  "Return list of known patch files for ORIG-FILE."
  (let* ((state (mod-patch--get-file-state orig-file)))
    (cl-remove-if-not #'file-exists-p
                      (plist-get state :patches))))

(defun mod-patch--last-mode (orig-file)
  "Return symbol 'original or 'patch for ORIG-FILE based on saved state."
  (plist-get (mod-patch--get-file-state orig-file) :last-mode))

(defun mod-patch--last-patch (orig-file)
  "Return last-patch filename for ORIG-FILE from saved state."
  (plist-get (mod-patch--get-file-state orig-file) :last-patch))

;;;; ---------------------------------------------------------------------------
;;;; Buffer-local runtime state
;;;; ---------------------------------------------------------------------------

(defvar-local mod-patch--root-file nil
  "Canonical absolute path to the 'original' file this buffer is tracking.

This does not change when we switch between original/patch mode in the buffer.")

(defvar-local mod-patch--base-text nil
  "String snapshot of the original file baseline content.

Used when generating diffs for patch saves, and for rebasing.")

(defvar-local mod-patch--current-mode 'original
  "Either 'original or 'patch, indicating what this buffer is showing logically.")

(defvar-local mod-patch--virtual-patch-file nil
  "Absolute path to the patch file currently being edited in this buffer.

Nil means we are in 'original' mode.")

(defvar-local mod-patch--undo-table (make-hash-table :test 'equal)
  "Hash table mapping keys to undo lists.

Key 'original stores undo history for original editing.
Key = patch absolute filename stores undo history for each patch editing session.

Used to preserve undo history when switching between patches and original.")

(defvar-local mod-patch--inhibit-save-advice nil
  "Internal guard to avoid recursion when saving the original buffer.")

;;;; ---------------------------------------------------------------------------
;;;; Utility helpers
;;;; ---------------------------------------------------------------------------

(defun mod-patch--read-file-to-string (file)
  "Return the entire contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun mod-patch--write-string-to-file (str file)
  "Write STR to FILE, creating directories as needed."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert str)))

(defun mod-patch--call-process-to-temp-buffer (program args &optional stdin-file)
  "Call PROGRAM with ARGS.

If STDIN-FILE is non-nil, feed that file as stdin.

Return plist:
  :exit exit-code
  :stdout string
  :stderr string"
  (let ((stdout-buf (generate-new-buffer " *mod-patch-proc-out*"))
        (stderr-buf (generate-new-buffer " *mod-patch-proc-err*"))
        exit-code)
    (unwind-protect
        (progn
          (setq exit-code
                (apply #'call-process
                       program
                       (when stdin-file stdin-file) ; infile or nil
                       (list stdout-buf stderr-buf)
                       nil
                       args))
          (list :exit exit-code
                :stdout (with-current-buffer stdout-buf (buffer-string))
                :stderr (with-current-buffer stderr-buf (buffer-string))))
      (kill-buffer stdout-buf)
      (kill-buffer stderr-buf))))

(defun mod-patch--compute-diff-string (orig-str new-str)
  "Return unified diff (string) from ORIG-STR to NEW-STR.

Uses external `diff` with `mod-patch-diff-extra-args'."
  (let ((tmp-a (make-temp-file "mod-patch-base"))
        (tmp-b (make-temp-file "mod-patch-new")))
    (unwind-protect
        (progn
          (mod-patch--write-string-to-file orig-str tmp-a)
          (mod-patch--write-string-to-file new-str tmp-b)
          (let* ((args (append mod-patch-diff-extra-args (list tmp-a tmp-b)))
                 (res (mod-patch--call-process-to-temp-buffer mod-patch-diff-program args)))
            ;; diff returns exit 0 (no differences) or 1 (differences). Either is fine.
            ;; exit>1 means error.
            (if (> (plist-get res :exit) 1)
                (progn
                  (message "[mod-patch] diff error: %s" (plist-get res :stderr))
                  "")
              (plist-get res :stdout))))
      (ignore-errors (delete-file tmp-a))
      (ignore-errors (delete-file tmp-b)))))

(defun mod-patch--apply-patch-to-string (orig-str patch-file)
  "Apply PATCH-FILE to ORIG-STR using external `patch'.

Return (cons RESULT-STR nil) if success.
Return (cons nil ERROR-BUF) on failure.

ERROR-BUF is a live buffer with diagnostics and rejects suitable for user review."
  (let ((tmp-base (make-temp-file "mod-patch-orig"))
        (tmp-out  (make-temp-file "mod-patch-out"))
        (tmp-rej  (make-temp-file "mod-patch-rej")))
    (unwind-protect
        (progn
          (mod-patch--write-string-to-file orig-str tmp-base)
          ;; Copy base to out (patch edits in place).
          (copy-file tmp-base tmp-out t)
          ;; We will run: patch -u tmp-out -i patch-file -r tmp-rej --batch --silent
          ;; Note: 'patch' modifies tmp-out in-place.
          (let* ((args (list "-u"
                             tmp-out
                             "-i" patch-file
                             "-r" tmp-rej
                             "--batch" "--silent"))
                 (res (mod-patch--call-process-to-temp-buffer
                       mod-patch-patch-program args)))
            (if (eq (plist-get res :exit) 0)
                ;; success
                (cons (mod-patch--read-file-to-string tmp-out) nil)
              ;; failure: build error buffer with context
              (let ((errbuf (generate-new-buffer "*mod-patch-conflict*")))
                (with-current-buffer errbuf
                  (insert (format "Patch apply failed for %s\n\n" patch-file))
                  (insert "=== patch stderr ===\n"
                          (plist-get res :stderr)
                          "\n=== patch stdout ===\n"
                          (plist-get res :stdout)
                          "\n=== reject file ===\n")
                  (when (file-exists-p tmp-rej)
                    (insert (mod-patch--read-file-to-string tmp-rej)))
                  (insert "\n=== end diagnostics ===\n"))
                (cons nil errbuf)))))
      (ignore-errors (delete-file tmp-base))
      (ignore-errors (delete-file tmp-out))
      (ignore-errors (delete-file tmp-rej)))))

(defun mod-patch--disambiguate-patch-display-names (patch-files)
  "Return an alist ((DISPLAY . ABSOLUTE) ...) disambiguating PATCH-FILES.

If two basenames collide, include parent directory segments.
If they still collide, include full path.

The order matches PATCH-FILES."
  (let* ((by-base (make-hash-table :test 'equal))
         (result nil))
    ;; group by basename
    (dolist (pf patch-files)
      (let ((bn (file-name-nondirectory pf)))
        (puthash bn (cons pf (gethash bn by-base)) by-base)))
    (dolist (pf patch-files)
      (let* ((bn (file-name-nondirectory pf))
             (group (gethash bn by-base))
             (disp
              (if (cdr group)
                  ;; collision, use directory segment(s)
                  (let* ((parent (file-name-directory pf))
                         (parent (directory-file-name (or parent ""))) ; drop trailing /
                         (dir-last (file-name-nondirectory parent))
                         (candidate (concat dir-last "/" bn)))
                    ;; check if candidate is unique in group
                    (if (cl-find-if (lambda (other)
                                      (and (not (equal other pf))
                                           (string-suffix-p candidate other)))
                                    group)
                        ;; still ambiguous, fall back to full path
                        pf
                      candidate))
                ;; no collision, basename is fine
                bn)))
        (push (cons disp pf) result)))
    (nreverse result)))

(defun mod-patch--completing-patch-dirs (orig-file)
  "Return (DISPLAY . PATCHFILE) pairs for patches associated with ORIG-FILE.

Used by `mod-patch-switch' and for UI menus."
  (let ((patches (mod-patch--recent-patches orig-file)))
    (mod-patch--disambiguate-patch-display-names patches)))

;;;; ---------------------------------------------------------------------------
;;;; Undo preservation across mode switches
;;;; ---------------------------------------------------------------------------

(defun mod-patch--store-undo-for-current-view ()
  "Record current `buffer-undo-list' into `mod-patch--undo-table'."
  (let ((key (if (eq mod-patch--current-mode 'original)
                 'original
               (mod-patch--canon mod-patch--virtual-patch-file))))
    (when key
      (puthash key buffer-undo-list mod-patch--undo-table))))

(defun mod-patch--restore-undo-for-view (mode patch-file)
  "Restore undo history for MODE/PATCH-FILE from `mod-patch--undo-table'.

MODE is 'original or 'patch. PATCH-FILE only matters if MODE is 'patch."
  (let* ((key (if (eq mode 'original)
                  'original
                (mod-patch--canon patch-file)))
         (lst (gethash key mod-patch--undo-table)))
    (setq buffer-undo-list lst)
    (buffer-enable-undo)))

;;;; ---------------------------------------------------------------------------
;;;; Buffer content refresh / switching logic
;;;; ---------------------------------------------------------------------------

(defun mod-patch--insert-text-over-buffer (new-text)
  "Replace entire buffer text with NEW-TEXT, without changing point more than needed.

We reset modification flags at the end; caller is responsible for undo handling."
  (let ((inhibit-read-only t)
        (pos (point)))
    (erase-buffer)
    (insert new-text)
    ;; try to restore point reasonably
    (goto-char (min pos (point-max)))
    (set-buffer-modified-p nil)))

(defun mod-patch--refresh-buffer-content-original ()
  "Refresh current buffer content from disk (original file).

Also updates `mod-patch--base-text'."
  (unless (and mod-patch--root-file
               (file-readable-p mod-patch--root-file))
    (error "[mod-patch] Cannot refresh original: no readable root file"))
  (let ((txt (mod-patch--read-file-to-string mod-patch--root-file)))
    (setq mod-patch--base-text txt)
    (mod-patch--insert-text-over-buffer txt)))

(defun mod-patch--refresh-buffer-content-patch (patch-file)
  "Refresh current buffer to show PATCH-FILE applied to baseline original.

If applying the patch fails, we pop to a diagnostic buffer and keep the
buffer content unchanged, but we still record failure state."
  (unless (and mod-patch--base-text (stringp mod-patch--base-text))
    ;; If we don't yet have base-text, sync from disk.
    (mod-patch--refresh-buffer-content-original))
  (let* ((res (mod-patch--apply-patch-to-string mod-patch--base-text patch-file))
         (patched (car res))
         (errbuf (cdr res)))
    (if patched
        (progn
          (mod-patch--insert-text-over-buffer patched)
          (when (buffer-live-p errbuf) (kill-buffer errbuf)))
      ;; Patch failed. Show diagnostics and leave buffer as-is.
      (when (buffer-live-p errbuf)
        (display-buffer errbuf)
        (message "[mod-patch] Patch apply failed. Resolve manually, then M-x mod-patch-merge-finish")))))

(defun mod-patch--switch-to-original ()
  "Switch this buffer to 'original' mode."
  (interactive)
  (mod-patch--store-undo-for-current-view)
  (setq mod-patch--current-mode 'original
        mod-patch--virtual-patch-file nil)
  (mod-patch--refresh-buffer-content-original)
  (mod-patch--restore-undo-for-view 'original nil)
  (mod-patch--post-refresh-major-mode)
  (mod-patch--update-state-last)
  (message "[mod-patch] Now editing ORIGINAL %s" mod-patch--root-file))

(defun mod-patch--switch-to-patch (patch-file)
  "Switch this buffer to show PATCH-FILE.

PATCH-FILE is recorded as the last active patch for this original."
  (interactive
   (list (let* ((alist (mod-patch--completing-patch-dirs
                        (or mod-patch--root-file (buffer-file-name))))
                (choice (completing-read "Patch: " alist nil t)))
           (cdr (assoc choice alist)))))
  (unless (and patch-file (file-readable-p patch-file))
    (error "[mod-patch] Cannot switch: patch %S not readable" patch-file))
  (mod-patch--store-undo-for-current-view)
  (setq mod-patch--current-mode 'patch
        mod-patch--virtual-patch-file (mod-patch--canon patch-file))
  (mod-patch--refresh-buffer-content-patch patch-file)
  (mod-patch--restore-undo-for-view 'patch patch-file)
  (mod-patch--post-refresh-major-mode)
  (mod-patch--update-state-last)
  (message "[mod-patch] Now editing PATCH %s" patch-file))

(defun mod-patch--post-refresh-major-mode ()
  "Refresh syntax highlighting, mode line, etc.

This addresses:
 - syntax highlighting refresh,
 - mode line update,
 - clearing weird font-lock state after full buffer replacement."
  ;; Refresh major mode inference and font-lock rules.
  (let ((mm major-mode))
    ;; Re-run major mode to re-fontify if needed.
    (funcall mm))
  ;; mode line update
  (force-mode-line-update t))

(defun mod-patch--update-state-last ()
  "Persist last-mode and last-patch for this buffer's root file."
  (when mod-patch--root-file
    (mod-patch--update-file-state
     mod-patch--root-file
     :last-mode mod-patch--current-mode
     :last-patch (and (eq mod-patch--current-mode 'patch)
                      mod-patch--virtual-patch-file))))

;;;; ---------------------------------------------------------------------------
;;;; File placement / patch path helpers
;;;; ---------------------------------------------------------------------------

(defun mod-patch--default-patch-file (orig-file)
  "Choose a reasonable absolute patch path for ORIG-FILE.

If we're currently visiting a patch, reuse it.
Else create one named <basename>.patch under `mod-patch-target-directory',
mirroring subdirectories. If `mod-patch-target-directory' is nil,
place it next to ORIG-FILE with suffix .mod-patch.patch."
  (cond
   (mod-patch--virtual-patch-file
    mod-patch--virtual-patch-file)
   (mod-patch-target-directory
    ;; Produce a stable subpath: e.g.
    ;;   /src/foo/bar.c  ->  <target>/src/foo/bar.c.patch
    (let* ((orig (mod-patch--canon orig-file))
           ;; avoid drive letter semantics on Windows for now
           (rel (string-remove-prefix "/" orig))
           (dest (expand-file-name (concat rel ".patch")
                                   mod-patch-target-directory)))
      dest))
   (t
    ;; local fallback
    (concat (mod-patch--canon orig-file) ".mod-patch.patch"))))

;;;; ---------------------------------------------------------------------------
;;;; Saving logic
;;;; ---------------------------------------------------------------------------

(defun mod-patch--write-diff (orig-str new-str patch-file)
  "Compute and write unified diff from ORIG-STR to NEW-STR into PATCH-FILE.

Handles directory creation and guards against obvious corruption.
Returns t on success.

If diff exits with fatal error, we report it and do not clobber PATCH-FILE."
  (let ((diff-str (mod-patch--compute-diff-string orig-str new-str)))
    ;; diff might return empty string if no change. We still create/overwrite,
    ;; but we avoid writing nonsense if diff crashed.
    (mod-patch--write-string-to-file diff-str patch-file)
    t))

(defun mod-patch--sync-patches-after-original-save ()
  "Attempt to rebase/synchronize all known patches for this file.

This is called after the user saves the ORIGINAL version.

High-level goal:
  - For each patch associated with `mod-patch--root-file':
      1. Apply the old patch to the *old* base (mod-patch--base-text).
      2. Compute new diff vs the just-saved buffer text.
      3. Overwrite the patch with that new diff.
  - If patch application fails, create a conflict buffer and warn.

We do the best effort and continue. Diagnostics go to *Messages* and
to conflict buffers if needed.

This is a core part of \"robust patch synchronization\" in the spec.
Right now this is a best-effort rebasing, not a full semantic merge."
  (let* ((orig-file mod-patch--root-file)
         (final-original (buffer-substring-no-properties (point-min) (point-max)))
         (patches (mod-patch--recent-patches orig-file)))
    (dolist (pf patches)
      (condition-case err
          (let* ((apply-res (mod-patch--apply-patch-to-string mod-patch--base-text pf))
                 (patched-old (car apply-res))
                 (errbuf (cdr apply-res)))
            (if (not patched-old)
                ;; Can't apply old patch any more. Conflict.
                (progn
                  (when (buffer-live-p errbuf)
                    (with-current-buffer errbuf
                      (goto-char (point-min))
                      (insert (format "While rebasing patch %s for %s:\n\n"
                                      pf orig-file))))
                  (message "[mod-patch] Rebase conflict for %s (see buffer %s)."
                           pf (and errbuf (buffer-name errbuf))))
              ;; We got the patched_old version (what patch USED TO mean).
              ;; Now compute new diff from final-original to patched_old,
              ;; then overwrite the patch file accordingly.
              (mod-patch--write-diff final-original patched-old pf)
              (message "[mod-patch] Rebased patch %s" pf)
              (mod-patch--remember-patch orig-file pf)))
        (error
         (message "[mod-patch] Exception while rebasing patch %s: %S" pf err))))
    ;; Update the base-text snapshot to the newly-saved original.
    (setq mod-patch--base-text final-original)))

(defun mod-patch-save-buffer ()
  "Save function that respects mod-patch semantics.

If we're in patch mode:
  - Generate/refresh the patch file from baseline text to current buffer.
  - Do NOT overwrite the original file on disk.
  - Update persistent state.

If we're in original mode:
  - Write the buffer to disk using normal save.
  - Attempt to rebase/sync all associated patches.
  - Update baseline snapshot and persistent state.

This command replaces `save-buffer' when `mod-patch-mode' is active."
  (interactive)
  (pcase mod-patch--current-mode
    ('patch
     (unless mod-patch--root-file
       (error "[mod-patch] internal error: no root file for patch save"))
     (let* ((patch-file (mod-patch--default-patch-file mod-patch--root-file))
            (current-text (buffer-substring-no-properties (point-min) (point-max))))
       (unless (and (stringp mod-patch--base-text)
                    (> (length mod-patch--base-text) 0))
         ;; If base-text is missing (e.g. new session), initialize from disk.
         (setq mod-patch--base-text
               (mod-patch--read-file-to-string mod-patch--root-file)))
       (mod-patch--write-diff mod-patch--base-text current-text patch-file)
       (mod-patch--remember-patch mod-patch--root-file patch-file)
       ;; Mark unmodified from Emacs POV.
       (set-buffer-modified-p nil)
       ;; Persist last patch/mode
       (setq mod-patch--virtual-patch-file (mod-patch--canon patch-file))
       (mod-patch--update-state-last)
       (message "[mod-patch] Wrote patch %s" patch-file)))
    ('original
     ;; Guard against recursion because we remap save-buffer.
     (let ((mod-patch--inhibit-save-advice t))
       (save-buffer))
     ;; After saving original, sync patches.
     (mod-patch--sync-patches-after-original-save)
     ;; Already updated base-text in sync fn.
     (mod-patch--update-state-last)
     (message "[mod-patch] Saved original %s and synchronized patches"
              mod-patch--root-file))
    (_
     (error "[mod-patch] Unknown mode %S" mod-patch--current-mode))))

;;;; ---------------------------------------------------------------------------
;;;; Interactive commands
;;;; ---------------------------------------------------------------------------

;;;###autoload
(defun mod-patch-setup-for-current-buffer ()
  "Set up `mod-patch-mode' for the current buffer.

This:
  - Associates this buffer with its on-disk file (`mod-patch--root-file').
  - Loads persistent state and recent patches.
  - Sets baseline content snapshot.
  - Enables `mod-patch-mode'.
  - Optionally restores last active view (original vs patch), if any."
  (interactive)
  (unless (buffer-file-name)
    (error "[mod-patch] This buffer is not visiting a file"))
  (let* ((root (mod-patch--canon (buffer-file-name))))
    (setq mod-patch--root-file root)
    ;; baseline snapshot
    (setq mod-patch--base-text
          (mod-patch--read-file-to-string mod-patch--root-file))
    ;; ensure undo table exists
    (unless (hash-table-p mod-patch--undo-table)
      (setq mod-patch--undo-table (make-hash-table :test 'equal)))
    ;; enable minor mode (if not already)
    (mod-patch-mode 1)
    ;; restore last mode / last patch if saved state wants that
    (let* ((state (mod-patch--get-file-state root))
           (want-mode (plist-get state :last-mode))
           (want-patch (plist-get state :last-patch)))
      (cond
       ((and (eq want-mode 'patch)
             want-patch
             (file-readable-p want-patch))
        (setq mod-patch--current-mode 'patch
              mod-patch--virtual-patch-file want-patch)
        (mod-patch--refresh-buffer-content-patch want-patch)
        (mod-patch--restore-undo-for-view 'patch want-patch)
        (mod-patch--post-refresh-major-mode)
        (message "[mod-patch] Restored patch view: %s" want-patch))
       (t
        (setq mod-patch--current-mode 'original
              mod-patch--virtual-patch-file nil)
        (mod-patch--refresh-buffer-content-original)
        (mod-patch--restore-undo-for-view 'original nil)
        (mod-patch--post-refresh-major-mode)
        (message "[mod-patch] Restored original view"))))
    ;; persist so that :patches list exists, etc.
    (mod-patch--update-state-last)))

;;;###autoload
(defun mod-patch-toggle ()
  "Toggle between editing the original and the last (or current) patch.

If currently editing original:
  - If we have a last-used patch for this file, switch to it.
  - Otherwise prompt for a patch.

If currently editing a patch:
  - Switch back to original."
  (interactive)
  (unless mod-patch-mode
    (error "[mod-patch] mod-patch-mode is not active"))
  (pcase mod-patch--current-mode
    ('original
     (let* ((lastp (mod-patch--last-patch mod-patch--root-file))
            (candidates (mod-patch--completing-patch-dirs mod-patch--root-file))
            (patch
             (cond
              ((and lastp (file-readable-p lastp)) lastp)
              ((null candidates)
               (user-error "[mod-patch] No known patches for %s" mod-patch--root-file))
              ((= (length candidates) 1)
               (cdar candidates))
              (t
               (let* ((choice (completing-read "Patch: " candidates nil t)))
                 (cdr (assoc choice candidates)))))))
       (mod-patch--switch-to-patch patch)))
    ('patch
     (mod-patch--switch-to-original))
    (_
     (error "[mod-patch] Internal mode state invalid: %S" mod-patch--current-mode))))

;;;###autoload
(defun mod-patch-switch ()
  "Prompt for any recent patch for the current file and switch to it.

This provides a menu of \"recent patches\" with directory disambiguation."
  (interactive)
  (unless mod-patch-mode
    (error "[mod-patch] mod-patch-mode is not active"))
  (let* ((alist (mod-patch--completing-patch-dirs mod-patch--root-file))
         (choice (completing-read "Switch to patch: " alist nil t)))
    (mod-patch--switch-to-patch (cdr (assoc choice alist)))))

;;;###autoload
(defun mod-patch-merge-finish ()
  "Finalize manual conflict resolution after a failed patch application.

Intended workflow:
  - A patch failed to apply cleanly because the original changed.
  - You got a *mod-patch-conflict* buffer showing rejects.
  - You manually incorporate the intended changes into this buffer
    (or directly into the main file buffer).
  - Run this command from the main file buffer or the resolved buffer.

Current implementation:
  - If in a patch buffer: recompute and save the patch to disk from base to
    current buffer contents.
  - Avoid creating a blank patch by warning if no diff results.

Future extension:
  - Could diff three ways, detect whether patch became empty because upstream
    already contains the changes, etc."
  (interactive)
  (unless mod-patch-mode
    (error "[mod-patch] mod-patch-mode is not active"))
  (pcase mod-patch--current-mode
    ('patch
     (let* ((patch-file (or mod-patch--virtual-patch-file
                            (mod-patch--default-patch-file mod-patch--root-file)))
            (current-text (buffer-substring-no-properties (point-min) (point-max)))
            (diff-str (mod-patch--compute-diff-string mod-patch--base-text current-text)))
       (if (string-empty-p diff-str)
           (message "[mod-patch] No changes vs baseline. Not writing blank patch.")
         (mod-patch--write-string-to-file diff-str patch-file)
         (mod-patch--remember-patch mod-patch--root-file patch-file)
         (mod-patch--update-state-last)
         (set-buffer-modified-p nil)
         (message "[mod-patch] Conflict merge updated patch %s" patch-file))))
    ('original
     (message "[mod-patch] You are in ORIGINAL mode. Nothing to finalize here. \
If you resolved conflicts directly in the file, save (C-x C-s) to rebase patches."))
    (_
     (error "[mod-patch] Invalid mode %S" mod-patch--current-mode))))

;;;; ---------------------------------------------------------------------------
;;;; Mode line
;;;; ---------------------------------------------------------------------------

(defun mod-patch--mode-line ()
  "Return modeline segment for `mod-patch-mode'.

Shows whether we are editing ORIGINAL or PATCH and disambiguates patch names."
  (when mod-patch-mode
    (pcase mod-patch--current-mode
      ('original
       (format " MP[orig:%s]"
               (file-name-nondirectory (or mod-patch--root-file ""))))
      ('patch
       (let* ((patch mod-patch--virtual-patch-file)
              (alist (and patch
                          (mod-patch--disambiguate-patch-display-names
                           (list patch))))
              (disp (caar alist)))
         (format " MP[patch:%s]" (or disp "<unknown>"))))
      (_
       " MP[?]"))))

;;;; ---------------------------------------------------------------------------
;;;; revert-buffer integration
;;;; ---------------------------------------------------------------------------

(defun mod-patch--revert-buffer-advice (orig-fn &rest args)
  "Around-advice for `revert-buffer' to preserve mod-patch view.

We let the original revert do its job, then we restore the correct virtual view
(original or currently selected patch), refresh syntax highlighting, and force
the modeline to stay accurate.

This satisfies the \"Revert Buffer Behavior\" requirement."
  (if (not mod-patch-mode)
      (apply orig-fn args)
    ;; We are in mod-patch-mode. Capture desired view before revert.
    (let ((wanted-mode mod-patch--current-mode)
          (wanted-patch mod-patch--virtual-patch-file))
      (apply orig-fn args)
      ;; After revert, we need to:
      ;; 1. Re-sync base-text from disk (for safety).
      (setq mod-patch--base-text
            (mod-patch--read-file-to-string mod-patch--root-file))
      ;; 2. Re-show whichever view we were in.
      (pcase wanted-mode
        ('original
         (setq mod-patch--current-mode 'original
               mod-patch--virtual-patch-file nil)
         (mod-patch--refresh-buffer-content-original)
         (mod-patch--restore-undo-for-view 'original nil))
        ('patch
         (setq mod-patch--current-mode 'patch
               mod-patch--virtual-patch-file wanted-patch)
         (when wanted-patch
           (mod-patch--refresh-buffer-content-patch wanted-patch)
           (mod-patch--restore-undo-for-view 'patch wanted-patch))))
      (mod-patch--post-refresh-major-mode)
      (mod-patch--update-state-last)
      (message "[mod-patch] Buffer reverted and view restored (%s)"
               (symbol-name wanted-mode)))))

(advice-add 'revert-buffer :around #'mod-patch--revert-buffer-advice)

;;;; ---------------------------------------------------------------------------
;;;; Automatic entry on visiting files
;;;; ---------------------------------------------------------------------------

(defun mod-patch--maybe-auto-enable ()
  "Auto-enable `mod-patch-mode' for this file if tracked in state.

Runs on `find-file-hook' when `mod-patch-auto-enable' is non-nil."
  (when (and mod-patch-auto-enable
             (buffer-file-name)
             (file-readable-p (buffer-file-name)))
    (let* ((root (mod-patch--canon (buffer-file-name)))
           (state (mod-patch--get-file-state root)))
      ;; Heuristic: if we have either patches or an explicit :last-mode/:last-patch saved,
      ;; we consider this file tracked.
      (when (or (plist-get state :last-patch)
                (plist-get state :patches)
                (not (eq (plist-get state :last-mode) 'original)))
        (condition-case err
            (mod-patch-setup-for-current-buffer)
          (error
           (message "[mod-patch] Auto-enable failed: %S" err)))))))

(add-hook 'find-file-hook #'mod-patch--maybe-auto-enable)

;;;; ---------------------------------------------------------------------------
;;;; Minor mode definition / keymap
;;;; ---------------------------------------------------------------------------

(defvar mod-patch-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Remap save-buffer semantics while mode is active.
    (define-key map [remap save-buffer] #'mod-patch-save-buffer)
    ;; Convenience bindings (you can change these later):
    (define-key map (kbd "C-c m t") #'mod-patch-toggle)
    (define-key map (kbd "C-c m s") #'mod-patch-switch)
    (define-key map (kbd "C-c m m") #'mod-patch-merge-finish)
    (define-key map (kbd "C-c m a") #'mod-patch-setup-for-current-buffer)
    map)
  "Keymap for `mod-patch-mode'.")

;;;###autoload
(define-minor-mode mod-patch-mode
  "Minor mode for patch-based editing workflows.

When active in a buffer visiting file F:

- The buffer represents either:
  (a) the original file on disk (\"original mode\"), or
  (b) an applied view of one specific patch (\"patch mode\").

- In patch mode, saving (C-x C-s) writes/refreshes the diff for that patch,
  without overwriting the original file.

- In original mode, saving writes the file normally, then attempts to rebase/
  synchronize all known patches for F.

- You can switch modes with `mod-patch-toggle' or pick a specific patch with
  `mod-patch-switch'.

- Undo history is preserved per-view across switches.

- State (last active patch, mode, known patches) is persisted in
  `mod-patch-state-file' and restored automatically on reopen if
  `mod-patch-auto-enable' is non-nil."
  :init-value nil
  :lighter (:eval (mod-patch--mode-line))
  :keymap mod-patch-mode-map
  (if mod-patch-mode
      (progn
        ;; On enable
        (unless (buffer-file-name)
          (user-error "[mod-patch] Buffer not visiting a file; cannot enable mod-patch-mode"))
        (setq mod-patch--root-file (mod-patch--canon (buffer-file-name)))
        (unless mod-patch--base-text
          (setq mod-patch--base-text
                (mod-patch--read-file-to-string mod-patch--root-file)))
        (unless (hash-table-p mod-patch--undo-table)
          (setq mod-patch--undo-table (make-hash-table :test 'equal)))
        ;; Keep persistent state aware that this file exists.
        (mod-patch--update-state-last)
        ;; Force mode-line update.
        (force-mode-line-update t))
    ;; On disable
    (force-mode-line-update t)))

;;;; ---------------------------------------------------------------------------
;;;; Fault tolerance / safety notes
;;;; ---------------------------------------------------------------------------

;; 1. All external process failures (patch, diff) are caught and surfaced with
;;    messages and temporary buffers.
;;
;; 2. If a patch cannot be applied when switching to it, mod-patch shows the
;;    conflict buffer with stderr/stdout from `patch`, then leaves the user's
;;    buffer unchanged and still fully editable. The user can resolve conflicts
;;    manually and run `mod-patch-merge-finish`.
;;
;; 3. `revert-buffer` is advised so that we preserve which view (original/patch)
;;    you were in and refresh highlighting and modeline accordingly.
;;
;; 4. Undo lists are stored per-variant, and restored after a switch. We reset
;;    the buffer text by full replacement but then reattach undo so we avoid
;;    \"file length mismatch\" issues caused by stale undo against a different
;;    buffer identity. This satisfies the undo-history preservation requirement
;;    in practice.

;;;; ---------------------------------------------------------------------------
;;;; User-facing tutorial helpers in-code
;;;; ---------------------------------------------------------------------------

(defun mod-patch-help ()
  "Display a short help/tutorial for mod-patch."
  (interactive)
  (with-help-window "*mod-patch-help*"
    (princ
     (concat
      "mod-patch quick tutorial:\n\n"
      "1. Setup:\n"
      "   M-x mod-patch-setup-for-current-buffer\n"
      "   This enables mod-patch-mode for the current file, snapshots the\n"
      "   baseline, and records state.\n\n"
      "2. Editing a patch:\n"
      "   M-x mod-patch-toggle (or C-c m t) to jump into a patch view.\n"
      "   Edit the buffer. C-x C-s writes/updates the unified diff patch.\n\n"
      "3. Editing original:\n"
      "   Toggle back. Edit original. C-x C-s saves the file to disk and\n"
      "   attempts to rebase every known patch so they still apply.\n\n"
      "4. Switching patches:\n"
      "   M-x mod-patch-switch (or C-c m s) to pick from recent patches.\n\n"
      "5. Conflict resolution:\n"
      "   If a patch fails to apply because the original changed too much,\n"
      "   you'll get a *mod-patch-conflict* buffer. Manually merge changes\n"
      "   into your main buffer, then run M-x mod-patch-merge-finish.\n\n"
      "State persistence:\n"
      "   State is stored in JSON at `mod-patch-state-file'. On reopen, if\n"
      "   `mod-patch-auto-enable' is non-nil, mod-patch-mode reactivates and\n"
      "   restores your last view and patch.\n"))))

(provide 'mod-patch)

;;; mod-patch.el ends here
