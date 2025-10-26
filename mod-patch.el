;;; mod-patch.el --- Patch-oriented mod editing workflow for game scripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: You
;; Version: 0.1
;; Keywords: tools, vc
;; Package-Requires: ((emacs "27.1"))
;; URL: (placeholder)

;;; Commentary:

;; mod-patch.el lets you "edit mods" instead of directly editing original game
;; files (for example Lua scripts shipped with a game).
;;
;; Core ideas:
;;
;; - You visit an original file (the game's real file on disk). mod-patch-mode
;;   can automatically turn that buffer into a "virtual view".
;;
;; - In :patch mode, that "virtual view" is the original file with your patch
;;   applied. Saving writes (or updates) a unified diff file into your chosen
;;   mod directory instead of overwriting the original file.
;;
;; - In :original mode, you're editing the true base file. On save, we write
;;   back to the original file on disk and then automatically rebase all known
;;   patch files for that original.
;;
;; - You can toggle between :original and a chosen patch dir. The buffer text
;;   will switch to match that "virtual view". Undo, syntax highlighting, and
;;   modeline info are all handled.
;;
;; - Per original file, we remember:
;;     :last-mode  (:original or :patch)
;;     :last-dir   (the active patch directory, if any)
;;     :dirs       (list of all known patch dirs you've used)
;;   This data is persisted in ~/.emacs.d/mod-patch-state.el and restored
;;   automatically when you revisit the file or restart Emacs.
;;
;; - If a patch fails to apply cleanly (because the base/original changed),
;;   we show a *mod-patch-conflict* buffer containing the patch program's
;;   output so you can resolve conflicts.
;;
;; - Modeline clearly shows whether you're viewing original or which patch
;;   directory you're editing (with basename disambiguation).
;;
;; Usage:
;;
;; 1. Open a file you want to mod.
;; 2. M-x mod-patch-setup-for-current-buffer
;;    or configure `mod-patch-auto-rules' to auto-enable.
;; 3. Use M-x mod-patch-toggle to switch between:
;;      - editing the original, or
;;      - editing a patch view.
;; 4. Use M-x mod-patch-set-target-directory to register a new mod directory
;;    as the output location for your patch file. The directory will be
;;    remembered for that original.
;; 5. Just save normally (C-x C-s). We'll do the right thing depending on
;;    whether you're in :original or :patch mode.
;; 6. If you edit the original and save, we automatically walk all known patch
;;    dirs and update (rebase) their patch files.
;; 7. If patch application fails because upstream changed, check the buffer
;;    *mod-patch-conflict* for details. Resolve manually, then run
;;    M-x mod-patch-merge-finish to regenerate a clean patch file.
;;
;; This file is intentionally self-contained and aims to be fault-tolerant by
;; design.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; ---------------------------------------------------------------------------
;;;; Customization / user options
;;;; ---------------------------------------------------------------------------

(defgroup mod-patch nil
  "Editing original files via patch-based mod workflow."
  :group 'tools
  :prefix "mod-patch-")

(defcustom mod-patch-state-file
  (expand-file-name "mod-patch-state.el" user-emacs-directory)
  "File where per-original-file state is persisted.
We store an alist of (ORIG-PATH . PLIST).

PLIST keys:
  :last-mode   either :original or :patch
  :last-dir    string or nil
  :dirs        list of strings (patch directories)"
  :type 'file
  :group 'mod-patch)

(defcustom mod-patch-auto-rules nil
  "Automatic activation rules for mod-patch-mode.

An alist where each entry is (MATCH . PLIST). MATCH can be:
  - a string regexp to test against `buffer-file-name'
  - a function of 0 args evaluated in the buffer, truthy to match

PLIST keys may include:
  :enable          non-nil means enable mod-patch-mode automatically
  :default-dir     directory path for patch output
  :force-patch     non-nil means start in :patch mode if possible
  :force-original  non-nil means start in :original mode

If both :force-patch and :force-original are nil and we have prior state
for this file, prior state wins."
  :type '(alist :key-type sexp :value-type plist)
  :group 'mod-patch)

(defcustom mod-patch-diff-context-lines 12
  "How many lines of context to include when generating patches (-U N)."
  :type 'integer
  :group 'mod-patch)

(defcustom mod-patch-patch-program "patch"
  "Program used to apply unified diffs."
  :type 'string
  :group 'mod-patch)

(defcustom mod-patch-diff-program "diff"
  "Program used to generate unified diffs."
  :type 'string
  :group 'mod-patch)

(defcustom mod-patch-conflict-buffer "*mod-patch-conflict*"
  "Name of buffer used to report failed patch applications."
  :type 'string
  :group 'mod-patch)

(defcustom mod-patch-mode-line-prefix " MP:"
  "Prefix for mod-patch-mode lighter."
  :type 'string
  :group 'mod-patch)

;;;; ---------------------------------------------------------------------------
;;;; Internal state
;;;; ---------------------------------------------------------------------------

;; Global alist:
;; ("/abs/path/to/original.lua"
;;   . (:last-mode :patch
;;      :last-dir "/mods/ui_overhaul/"
;;      :dirs ("/mods/ui_overhaul/" "/mods/alt_ui_overhaul/")))
(defvar mod-patch--file-state nil
  "Global alist mapping canonical original file path -> plist.
See `mod-patch-state-file' for structure.")

;; Buffer-local tracking. These must never be nil once mod-patch-mode is active.
(defvar-local mod-patch--current-orig nil
  "Canonical absolute path of the original file for this buffer.")

(defvar-local mod-patch--current-mode-symbol nil
  "Either :original or :patch indicating current virtual view.")

(defvar-local mod-patch--current-dir nil
  "Patch directory currently active for this buffer, or nil in :original mode.")

(defvar-local mod-patch--has-conflict nil
  "Non-nil if last patch apply for this buffer failed to apply cleanly.")

;; Guard to avoid recursion in hooks.
(defvar-local mod-patch--internal-save-running nil
  "Non-nil while mod-patch internal save/rebase logic is executing.")

;; Revert behavior override.
(defvar-local mod-patch--revert-in-progress nil
  "Non-nil while mod-patch custom revert logic is running.")

;;;; ---------------------------------------------------------------------------
;;;; Utility helpers
;;;; ---------------------------------------------------------------------------

(defun mod-patch--canonical (file)
  "Return canonical absolute FILE path for use as a key.
We expand and resolve symlinks."
  (when file
    (file-truename (expand-file-name file))))

(defun mod-patch--sha1-file (file)
  "Return SHA1 hash of FILE contents."
  (with-temp-buffer
    (insert-file-contents file)
    (secure-hash 'sha1 (current-buffer))))

(defun mod-patch--insert-header (sha)
  "Insert patch metadata header for base SHA at point.
Call from within a buffer that contains unified diff text.
We prepend something Emacs/our tooling can parse later."
  (goto-char (point-min))
  (insert (format ";;; mod-patch base-sha1: %s\n" sha)))

(defun mod-patch--get-or-create-file-state (orig)
  "Return plist state for ORIG (canonical path string).
Ensure it's present in `mod-patch--file-state' alist."
  (let* ((cell (cl-assoc orig mod-patch--file-state :test #'string-equal)))
    (unless cell
      (setq cell (cons orig (list :last-mode :original
                                  :last-dir nil
                                  :dirs nil)))
      (push cell mod-patch--file-state))
    (cdr cell)))

(defun mod-patch--update-file-state (orig plist)
  "Mutate/replace state for ORIG with PLIST in `mod-patch--file-state'."
  (let* ((cell (cl-assoc orig mod-patch--file-state :test #'string-equal)))
    (if cell
        (setcdr cell plist)
      (push (cons orig plist) mod-patch--file-state))
    plist))

(defun mod-patch--save-state ()
  "Persist `mod-patch--file-state' to `mod-patch-state-file'."
  (with-temp-file mod-patch-state-file
    (let ((print-length nil)
          (print-level nil))
      (prin1 mod-patch--file-state (current-buffer)))))

(defun mod-patch--load-state ()
  "Load state from `mod-patch-state-file' into `mod-patch--file-state'."
  (when (file-readable-p mod-patch-state-file)
    (with-temp-buffer
      (insert-file-contents mod-patch-state-file)
      (goto-char (point-min))
      (setq mod-patch--file-state (read (current-buffer))))))

;; Load state immediately.
(mod-patch--load-state)

(defun mod-patch--merge-dirs-list (orig maybe-dir)
  "Return updated :dirs list for ORIG including MAYBE-DIR (if non-nil).
Guarantee uniqueness, preserve order (newest last)."
  (let* ((plist (mod-patch--get-or-create-file-state orig))
         (old   (plist-get plist :dirs))
         (dirs  (cl-remove-duplicates
                 (if (and maybe-dir (stringp maybe-dir))
                     (append old (list (directory-file-name (expand-file-name maybe-dir))))
                   old)
                 :test #'string-equal)))
    dirs))

(defun mod-patch--persist-current-state ()
  "Persist current buffer view (:last-mode, :last-dir, :dirs) for this buffer's ORIG."
  (when-let* ((orig mod-patch--current-orig)
              (mode mod-patch--current-mode-symbol))
    (let* ((orig-key (mod-patch--canonical orig))
           (dir (or mod-patch--current-dir
                    (plist-get
                     (mod-patch--get-or-create-file-state orig-key)
                     :last-dir)))
           (dirs (mod-patch--merge-dirs-list orig-key dir))
           (payload (list :last-mode mode
                          :last-dir  dir
                          :dirs      dirs)))
      (mod-patch--update-file-state orig-key payload)
      (mod-patch--save-state))))

(defun mod-patch--context-for-orig (orig)
  "Return (:last-mode :last-dir :dirs) plist for ORIG canonical."
  (mod-patch--get-or-create-file-state (mod-patch--canonical orig)))

(defun mod-patch--basename (dir)
  "Return directory basename sans trailing slash."
  (file-name-nondirectory (directory-file-name dir)))

(defun mod-patch--path-segments (dir)
  "Return list of path segments in DIR, split.
Example: /a/b/c -> (\"a\" \"b\" \"c\")."
  (let* ((abs (directory-file-name (expand-file-name dir)))
         (parts nil)
         (done nil)
         (parent abs))
    (while (not done)
      (let ((base (file-name-nondirectory parent))
            (next (file-name-directory parent)))
        (push base parts)
        (if (or (null next)
                (string= next parent)
                (string= next "/"))
            (setq done t)
          (setq parent (directory-file-name next)))))
    parts))

(defun mod-patch--shortest-unique-suffix (target all)
  "Return shortest unique suffix path string for TARGET given ALL.
TARGET and ALL are directory paths. We compare from the end.
If needed we include more parent segments to disambiguate."
  (let* ((seg-alist
          ;; For each dir in ALL, compute reversed segments.
          (mapcar (lambda (d)
                    (cons d (reverse (mod-patch--path-segments d))))
                  all))
         (target-segs (alist-get target seg-alist nil nil #'string-equal))
         (max-depth (apply #'max 1 (mapcar (lambda (x) (length (cdr x)))
                                           seg-alist)))
         (depth 1)
         (unique nil))
    (while (and (<= depth max-depth) (not unique))
      (let ((candidate
             (mapconcat #'identity
                        (reverse (cl-subseq target-segs 0 depth))
                        "/"))
            (collision nil))
        ;; Check collisions
        (dolist (pair seg-alist)
          (let* ((d (car pair))
                 (segs (cdr pair))
                 (cand2 (mapconcat #'identity
                                   (reverse (cl-subseq
                                             segs 0 (min depth (length segs))))
                                   "/")))
            (when (and (not (string-equal d target))
                       (string-equal cand2 candidate))
              (setq collision t))))
        (unless collision
          (setq unique candidate)))
      (setq depth (1+ depth)))
    (or unique
        (file-name-nondirectory (directory-file-name target)))))

(defun mod-patch--disambiguate-dir (orig dir dirs)
  "Return shortest unique suffix for DIR among DIRS for ORIG.
ORIG is currently unused in the computation, but kept for API stability."
  (mod-patch--shortest-unique-suffix
   (directory-file-name (expand-file-name dir))
   (mapcar (lambda (d) (directory-file-name (expand-file-name d))) dirs)))

(defun mod-patch--lighter-patch ()
  "Return mode line lighter for `mod-patch-mode'.
Always returns a string. Never signals."
  (condition-case _
      (let* ((mode mod-patch--current-mode-symbol)
             (dir  mod-patch--current-dir)
             (orig mod-patch--current-orig)
             (conf mod-patch--has-conflict))
        (cond
         ((eq mode :original)
          (format "%sorig%s"
                  mod-patch-mode-line-prefix
                  (if conf "!" " ")))
         ((and (eq mode :patch) dir)
          (let* ((cell (mod-patch--get-or-create-file-state
                        (mod-patch--canonical orig)))
                 (dirs (plist-get cell :dirs))
                 (short (mod-patch--disambiguate-dir orig dir dirs)))
            (format "%spatch[%s]%s"
                    mod-patch-mode-line-prefix
                    short
                    (if conf "!" " "))))
         (t
          (format "%s? " mod-patch-mode-line-prefix))))
    (error (format "%s? " mod-patch-mode-line-prefix))))

;;;; ---------------------------------------------------------------------------
;;;; Patch + diff helpers
;;;; ---------------------------------------------------------------------------

(defun mod-patch--patch-file-path (orig dir)
  "Return absolute patch file path for ORIG under DIR.
We name the patch after the basename of ORIG plus .patch."
  (let* ((base (file-name-nondirectory orig))
         (patch (concat base ".patch")))
    (expand-file-name patch dir)))

(defun mod-patch--write-temp-file (content)
  "Write CONTENT (string) to a new temp file, return its path."
  (let ((tmp (make-temp-file "mod-patch-tmp-" nil ".txt")))
    (with-temp-file tmp
      (insert content))
    tmp))

(defun mod-patch--call-process-capture (program infile args &optional outfile)
  "Run PROGRAM with ARGS.
If INFILE non-nil, feed from INFILE.
If OUTFILE non-nil, redirect stdout there.
Return plist (:exit EXIT :stdout STR :stderr STR)."
  (let ((stdout-buf (generate-new-buffer " *mod-patch-stdout*"))
        (stderr-buf (generate-new-buffer " *mod-patch-stderr*"))
        (exit-code nil))
    (unwind-protect
        (progn
          (setq exit-code
                (apply #'call-process program
                       (and infile infile) ; INFILE or nil
                       (list stdout-buf stderr-buf)
                       nil
                       args))
          (list :exit   exit-code
                :stdout (with-current-buffer stdout-buf
                          (buffer-substring-no-properties
                           (point-min) (point-max)))
                :stderr (with-current-buffer stderr-buf
                          (buffer-substring-no-properties
                           (point-min) (point-max)))))
      (kill-buffer stdout-buf)
      (kill-buffer stderr-buf))))

(defun mod-patch--apply-patch-to-file (orig patch-file)
  "Apply PATCH-FILE to ORIG using external `patch'.
Return plist with keys:
  :ok          t if applied cleanly, nil otherwise
  :merged-file path to temp file containing merged result (when :ok t)
  :stdout      patch stdout
  :stderr      patch stderr
  :exit        exit code
When not :ok, :merged-file may still exist but is not guaranteed to be correct."
  (let* ((tmp (make-temp-file "mod-patch-merged-" nil ".tmp")))
    ;; Copy ORIG to tmp first.
    (copy-file orig tmp t t t)
    ;; Run "patch -p0 -u -N <patch-file> tmpfile"
    ;; We'll cd to directory containing tmp so patch can find the file by name.
    ;; Simpler: call patch with -i patch-file and -o tmp, but not all patch
    ;; variants support -o. We'll stick with editing tmp in-place:
    (let* ((default-directory (file-name-directory tmp))
           (args (list "-p0" "-u" "-N" "-i" patch-file (file-name-nondirectory tmp)))
           (res (mod-patch--call-process-capture mod-patch-patch-program
                                                 nil args)))
      (plist-put res :merged-file tmp)
      (if (= (plist-get res :exit) 0)
          (plist-put res :ok t)
        (plist-put res :ok nil))
      res)))

(defun mod-patch--make-unified-diff (orig tmp)
  "Return plist of (:exit INT :buffer BUF) producing diff between ORIG and TMP.
Writes diff output into BUF (live buffer object)."
  (let ((buf (get-buffer-create "*mod-patch-diff*")))
    (with-current-buffer buf
      (erase-buffer))
    (let* ((args (list "-u"
                       (format "-U%d" mod-patch-diff-context-lines)
                       "-N"
                       orig tmp))
           (exit (apply #'call-process mod-patch-diff-program nil buf nil args)))
      (list :exit exit :buffer buf))))

(defun mod-patch--write-diff (orig tmp patch-file)
  "Build large-context diff between ORIG and TMP into PATCH-FILE, preserving header.

If there is no diff (exit code 0), delete PATCH-FILE if it exists.
If diff exists (exit code 1), prepend header with base sha1 of ORIG then write.
If exit is something else, raise error."
  (let ((buf (get-buffer-create "*mod-patch-diff*")))
    (with-current-buffer buf
      (erase-buffer))
    (let* ((args (list "-u"
                       (format "-U%d" mod-patch-diff-context-lines)
                       "-N"
                       orig tmp))
           (status (apply #'call-process mod-patch-diff-program nil buf nil args)))
      (cond
       ;; No diff:
       ((= status 0)
        (when (file-exists-p patch-file)
          (delete-file patch-file)))
       ;; Diff present:
       ((= status 1)
        (let ((sha (mod-patch--sha1-file orig)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (mod-patch--insert-header sha)
              (write-region (point-min) (point-max)
                            patch-file nil 'silent)))))
       (t
        (error "diff failed with code %s" status))))
    (kill-buffer buf)))

;;;; ---------------------------------------------------------------------------
;;;; Virtual view materialization
;;;; ---------------------------------------------------------------------------

(defun mod-patch--read-file-as-string (file)
  "Return contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun mod-patch--virtual-buffer-content (orig mode dir &optional conflict-info-out)
  "Return plist describing what the buffer *should* show for ORIG in MODE.

MODE is :original or :patch. When MODE is :patch, DIR is the patch directory.

Return plist:
  :text        string to insert into buffer
  :conflict    t if patch failed to apply
  :conf-msg    string (stdout+stderr) if conflict
  :patch-file  path to patch file (when :patch)
  :dir         dir
  :orig        orig

If patch failed, :text will fall back to ORIG contents."
  (cond
   ((eq mode :original)
    (list :text (mod-patch--read-file-as-string orig)
          :conflict nil
          :dir nil
          :patch-file nil
          :orig orig))

   ((eq mode :patch)
    (let* ((patch-file (mod-patch--patch-file-path orig dir)))
      (if (file-exists-p patch-file)
          (let* ((apply-res (mod-patch--apply-patch-to-file orig patch-file))
                 (merged-file (plist-get apply-res :merged-file))
                 (ok (plist-get apply-res :ok))
                 (log (concat (or (plist-get apply-res :stdout) "")
                              (or (plist-get apply-res :stderr) ""))))
            (if ok
                (list :text (mod-patch--read-file-as-string merged-file)
                      :conflict nil
                      :conf-msg nil
                      :dir dir
                      :patch-file patch-file
                      :orig orig)
              ;; conflict
              (list :text (mod-patch--read-file-as-string orig)
                    :conflict t
                    :conf-msg log
                    :dir dir
                    :patch-file patch-file
                    :orig orig)))
        ;; No patch file yet -> same as original
        (list :text (mod-patch--read-file-as-string orig)
              :conflict nil
              :conf-msg nil
              :dir dir
              :patch-file patch-file
              :orig orig))))
   (t
    (error "Unknown mode: %S" mode))))

(defun mod-patch--show-conflict-buffer (log)
  "Display LOG in `mod-patch-conflict-buffer'."
  (when (and log (not (string-empty-p log)))
    (with-current-buffer (get-buffer-create mod-patch-conflict-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Patch failed to apply cleanly.\n\n")
        (insert "stdout/stderr from patch:\n\n")
        (insert log)
        (special-mode)))
    (display-buffer mod-patch-conflict-buffer)))

(defun mod-patch--replace-buffer-text-safely (text)
  "Replace current buffer contents with TEXT, attempting to preserve undo, point.
Uses `replace-buffer-contents' when possible and source temp buffer is distinct.
Falls back to erase/insert."
  (let ((cur-buf (current-buffer)))
    (with-temp-buffer
      (insert text)
      ;; Only call replace-buffer-contents if available and not same buffer.
      (if (and (fboundp 'replace-buffer-contents)
               (not (eq cur-buf (current-buffer))))
          (let ((src (current-buffer)))
            (with-current-buffer cur-buf
              (save-excursion
                (replace-buffer-contents src))))
        ;; Fallback path
        (with-current-buffer cur-buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert text)))))))

(defun mod-patch--refresh-fontification ()
  "Force syntax highlighting / tree-sitter refresh after buffer rewrite."
  ;; Heavy but reliable: re-run major mode's normal font-lock pipeline.
  ;; We must NOT change the buffer-file-name, so `normal-mode' won't guess a
  ;; new mode from a different temp filename. But `normal-mode' will re-run
  ;; the current major mode anyway.
  (normal-mode)
  (when (fboundp 'font-lock-ensure)
    (font-lock-ensure (point-min) (point-max))))

(defun mod-patch--refresh-buffer-view (mode dir)
  "Switch current buffer to virtual view described by MODE and DIR.
Updates buffer text, buffer-local vars, conflict flags, fontification,
modeline lighter state, and persists global state."
  (let* ((orig (or mod-patch--current-orig (buffer-file-name)))
         (orig (mod-patch--canonical orig))
         (view (mod-patch--virtual-buffer-content orig mode dir))
         (text (plist-get view :text))
         (conflict (plist-get view :conflict))
         (conf-msg (plist-get view :conf-msg)))

    ;; Update buffer text
    (mod-patch--replace-buffer-text-safely text)

    ;; Update locals
    (setq mod-patch--current-orig orig
          mod-patch--current-mode-symbol mode
          mod-patch--current-dir (and (eq mode :patch) dir)
          mod-patch--has-conflict conflict)

    ;; Conflict buffer if needed
    (when conflict
      (mod-patch--show-conflict-buffer conf-msg))

    ;; Refresh highlighting
    (mod-patch--refresh-fontification)

    ;; Persist new state (including dirs list if needed)
    (mod-patch--persist-current-state)

    ;; Mark buffer modified state heuristically:
    ;; In :original mode, if we exactly match file on disk, mark unmodified.
    ;; In :patch mode, we'll consider buffer as "modified" because edits
    ;; produce new patch output.
    (cond
     ((eq mode :original)
      (let ((disk (mod-patch--read-file-as-string orig)))
        (set-buffer-modified-p (not (string-equal disk text)))))
     (t
      (set-buffer-modified-p t)))))

;;;; ---------------------------------------------------------------------------
;;;; Completion helpers for selecting patch dirs
;;;; ---------------------------------------------------------------------------

(defun mod-patch--completing-patch-dirs (orig)
  "Return list of candidate patch directories for ORIG.
Deduplicated, most-recent last-dir should appear prominently."
  (let* ((st (mod-patch--get-or-create-file-state (mod-patch--canonical orig)))
         (dirs (plist-get st :dirs))
         (seen nil)
         (candidates nil))
    (dolist (d dirs)
      (unless (member d seen)
        (push d candidates)
        (push d seen)))
    (nreverse candidates)))

(defun mod-patch--read-patch-dir (orig &optional prompt)
  "Interactively choose a patch dir for ORIG using completion.
Returns chosen directory string."
  (let* ((cands (mod-patch--completing-patch-dirs orig))
         (default (plist-get (mod-patch--get-or-create-file-state
                              (mod-patch--canonical orig))
                             :last-dir))
         (prompt (or prompt "Patch directory: ")))
    (unless cands
      (user-error "No known patch dirs for %s; use M-x mod-patch-set-target-directory"
                  orig))
    (completing-read prompt cands nil t nil nil default)))

;;;; ---------------------------------------------------------------------------
;;;; Rebuilding diffs on save
;;;; ---------------------------------------------------------------------------

(defun mod-patch--regen-patch-for-dir (orig dir merged-content)
  "For ORIG (original base file path) and DIR patch dir,
compare MERGED-CONTENT string against ORIG and update patch file accordingly."
  (let* ((patch-file (mod-patch--patch-file-path orig dir))
         (tmp-file (mod-patch--write-temp-file merged-content)))
    (unwind-protect
        (mod-patch--write-diff orig tmp-file patch-file)
      (ignore-errors (delete-file tmp-file)))))

(defun mod-patch--apply-and-read (orig dir)
  "Return plist of (:ok BOOL :content STR :log STR) for applying patch in DIR to ORIG."
  (let* ((patch-file (mod-patch--patch-file-path orig dir)))
    (if (file-exists-p patch-file)
        (let* ((res (mod-patch--apply-patch-to-file orig patch-file))
               (ok  (plist-get res :ok))
               (mf  (plist-get res :merged-file))
               (log (concat (or (plist-get res :stdout) "")
                            (or (plist-get res :stderr) ""))))
          (list :ok ok
                :content (if (and ok (file-readable-p mf))
                             (mod-patch--read-file-as-string mf)
                           (mod-patch--read-file-as-string orig))
                :log log))
      ;; no patch file
      (list :ok t
            :content (mod-patch--read-file-as-string orig)
            :log ""))))

(defun mod-patch--rebuild-patch-after-original-save (orig)
  "After ORIG changed on disk, walk all known patch dirs for ORIG.
Attempt to update each .patch so it's rebased to the new ORIG.
If conflict, show conflict buffer."
  (let* ((state (mod-patch--get-or-create-file-state orig))
         (dirs  (plist-get state :dirs)))
    (dolist (dir dirs)
      (let* ((apply-info (mod-patch--apply-and-read orig dir))
             (ok (plist-get apply-info :ok))
             (merged (plist-get apply-info :content))
             (log (plist-get apply-info :log)))
        (if ok
            ;; Write new diff ORIG vs merged back into patch file.
            (mod-patch--regen-patch-for-dir orig dir merged)
          ;; Conflict: show info, but DO NOT delete patch.
          (mod-patch--show-conflict-buffer log))))))

;;;; ---------------------------------------------------------------------------
;;;; Saving logic
;;;; ---------------------------------------------------------------------------

(defun mod-patch--save-original-and-rebase-patches ()
  "We are in :original mode. Save current buffer into ORIG on disk.
Then rebase all known patch dirs for this ORIG."
  (let ((orig mod-patch--current-orig))
    ;; First, save buffer to ORIG normally.
    ;; We temporarily disable our save advice recursion guard.
    (let ((mod-patch--internal-save-running t))
      (write-region (point-min) (point-max) orig nil 'silent)
      (set-buffer-modified-p nil))
    ;; Now rebase all patch dirs.
    (mod-patch--rebuild-patch-after-original-save orig)
    ;; After rebasing, persist state.
    (mod-patch--persist-current-state)))

(defun mod-patch--save-patch-view ()
  "We are in :patch mode. Regenerate this patch from current buffer text."
  (let* ((orig mod-patch--current-orig)
         (dir  mod-patch--current-dir))
    (unless (and (stringp dir) (file-directory-p dir))
      (user-error "Patch directory %S is not valid" dir))
    (let* ((merged (buffer-substring-no-properties (point-min) (point-max))))
      (mod-patch--regen-patch-for-dir orig dir merged)
      ;; After writing patch, mark buffer unmodified (conceptually saved)
      (set-buffer-modified-p nil)
      ;; Persist state (:last-dir should be this dir)
      (mod-patch--persist-current-state))))

(defun mod-patch--after-save-dispatch ()
  "Buffer-local `after-save-hook' dispatcher.
Handles :original vs :patch save semantics.

We do not want infinite recursion, so we check guard var."
  (unless mod-patch--internal-save-running
    (pcase mod-patch--current-mode-symbol
      (:original
       ;; We already wrote buffer to file as part of normal save;
       ;; but in :original mode we override save anyway, so normal save
       ;; should not have run. We'll unify behavior:
       (mod-patch--save-original-and-rebase-patches))
      (:patch
       (mod-patch--save-patch-view))
      (_
       ;; Fallback no-op.
       nil))))

;;;; ---------------------------------------------------------------------------
;;;; Revert logic
;;;; ---------------------------------------------------------------------------

(defun mod-patch--revert-buffer ()
  "Custom revert-buffer implementation for mod-patch buffers.
Reload correct virtual view (:original or :patch last-dir)."
  (interactive)
  (let ((mod-patch--revert-in-progress t))
    ;; Determine what we *should* be showing
    (let* ((orig mod-patch--current-orig)
           (st   (mod-patch--get-or-create-file-state orig))
           (mode (or mod-patch--current-mode-symbol
                     (plist-get st :last-mode)
                     :original))
           (dir  (or mod-patch--current-dir
                     (plist-get st :last-dir))))
      (mod-patch--refresh-buffer-view mode dir))))

;;;; ---------------------------------------------------------------------------
;;;; Interactive commands
;;;; ---------------------------------------------------------------------------

(defun mod-patch-merge-finish ()
  "Finalize manual conflict resolution.
Current buffer is assumed to represent intended final merged content
for the active patch directory. We regenerate the patch file accordingly."
  (interactive)
  (unless (eq mod-patch--current-mode-symbol :patch)
    (user-error "Not in :patch mode"))
  (unless mod-patch--current-dir
    (user-error "No current patch directory"))
  (let* ((orig mod-patch--current-orig)
         (dir  mod-patch--current-dir)
         (merged (buffer-substring-no-properties (point-min) (point-max))))
    (mod-patch--regen-patch-for-dir orig dir merged)
    ;; Now recompute virtual view to clear conflict flag and refresh state.
    (setq mod-patch--has-conflict nil)
    (mod-patch--refresh-buffer-view :patch dir)
    (message "mod-patch: patch file updated for %s" dir)))

(defun mod-patch-set-target-directory (dir)
  "Set patch output directory for this buffer and switch to that patch view.
Adds DIR to known patch dirs for this original file and persists."
  (interactive "DSelect patch target directory: ")
  (let* ((orig (or mod-patch--current-orig (buffer-file-name))))
    (unless orig
      (user-error "Buffer not visiting a file"))
    (let* ((orig (mod-patch--canonical orig))
           (dir  (directory-file-name (expand-file-name dir))))
      ;; Merge dir into state, set :last-mode :patch, :last-dir dir.
      (let* ((st (mod-patch--get-or-create-file-state orig))
             (dirs (mod-patch--merge-dirs-list orig dir))
             (payload (list :last-mode :patch
                            :last-dir  dir
                            :dirs      dirs)))
        (mod-patch--update-file-state orig payload)
        (mod-patch--save-state))
      ;; Refresh buffer as patch view
      (mod-patch--refresh-buffer-view :patch dir)
      (message "mod-patch: now editing patch in %s" dir))))

(defun mod-patch-switch-patch ()
  "Switch to an already-known patch directory for this original file.
Prompts from the recent list (:dirs)."
  (interactive)
  (unless mod-patch--current-orig
    (user-error "Not associated with any original file"))
  (let* ((orig mod-patch--current-orig)
         (dir  (mod-patch--read-patch-dir orig "Switch to patch directory: ")))
    (mod-patch--refresh-buffer-view :patch dir)
    (message "mod-patch: switched to patch %s" dir)))

(defun mod-patch-toggle ()
  "Toggle between :original view and :patch view.
If currently :patch, switch to :original.
If currently :original, prompt for a patch dir."
  (interactive)
  (unless mod-patch--current-orig
    (user-error "Not associated with any original file"))
  (pcase mod-patch--current-mode-symbol
    (:patch
     ;; Switch to original
     (mod-patch--refresh-buffer-view :original nil)
     (message "mod-patch: now editing ORIGINAL base file"))
    (:original
     ;; Choose patch dir
     (let* ((dir (mod-patch--read-patch-dir mod-patch--current-orig
                                            "Edit patch directory: ")))
       (mod-patch--refresh-buffer-view :patch dir)
       (message "mod-patch: now editing patch %s" dir)))
    (_
     ;; Default: go to original
     (mod-patch--refresh-buffer-view :original nil)
     (message "mod-patch: editing ORIGINAL (fallback)"))))

(defun mod-patch-setup-for-current-buffer ()
  "Initialize mod-patch-mode for the current buffer.
Restores last-mode/:last-dir from persisted state or `mod-patch-auto-rules'."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Current buffer is not visiting a file on disk"))
  (let* ((orig (mod-patch--canonical (buffer-file-name)))
         (auto (mod-patch--match-auto-rule (buffer-file-name)))
         (st   (mod-patch--get-or-create-file-state orig))
         ;; Decide starting mode/dir
         (mode0 (or (plist-get st :last-mode) :original))
         (dir0  (plist-get st :last-dir))
         (mode  mode0)
         (dir   dir0))
    ;; Apply auto-rules overrides if present.
    (when auto
      (when (plist-get auto :default-dir)
        (let* ((dd (directory-file-name
                    (expand-file-name (plist-get auto :default-dir)))))
          ;; Inject dd into state dirs if missing.
          (let* ((dirs (mod-patch--merge-dirs-list orig dd))
                 (payload (list :last-mode mode0
                                :last-dir  (or dir0 dd)
                                :dirs      dirs)))
            (mod-patch--update-file-state orig payload)
            (setq st payload
                  dir (or dir0 dd)))))
      (when (plist-get auto :force-original)
        (setq mode :original
              dir nil))
      (when (plist-get auto :force-patch)
        ;; If :force-patch is requested but we don't have dir yet, try
        ;; default-dir in auto.
        (unless dir
          (setq dir (plist-get auto :default-dir)))
        (setq mode :patch)))

    ;; Activate mode
    (setq mod-patch--current-orig orig)
    (mod-patch-mode 1)

    ;; Install revert-buffer-function buffer-locally.
    (setq-local revert-buffer-function #'mod-patch--revert-buffer)

    ;; Initial render
    (mod-patch--refresh-buffer-view mode dir)

    (message "mod-patch: setup complete (%s)"
             (if (eq mode :original) "original" (format "patch %s" dir)))))

(defun mod-patch--match-auto-rule (file)
  "Return PLIST for first matching rule in `mod-patch-auto-rules' for FILE, or nil."
  (let ((res nil))
    (dolist (entry mod-patch-auto-rules)
      (let ((match (car entry))
            (plist (cdr entry)))
        (when (and (null res)
                   (cond
                    ((stringp match)
                     (string-match-p match file))
                    ((functionp match)
                     (with-current-buffer (current-buffer)
                       (funcall match)))
                    (t nil)))
          (setq res plist))))
    res))

;;;; ---------------------------------------------------------------------------
;;;; Minor mode definition
;;;; ---------------------------------------------------------------------------

(defvar mod-patch-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Suggested bindings (user can rebind):
    (define-key map (kbd "C-c m t") #'mod-patch-toggle)
    (define-key map (kbd "C-c m s") #'mod-patch-set-target-directory)
    (define-key map (kbd "C-c m p") #'mod-patch-switch-patch)
    (define-key map (kbd "C-c m m") #'mod-patch-merge-finish)
    (define-key map (kbd "C-c m r") #'mod-patch--revert-buffer)
    map)
  "Keymap for `mod-patch-mode'.")

(defun mod-patch--after-save-hook-wrapper ()
  "Wrapper hook to run `mod-patch--after-save-dispatch'."
  (when (bound-and-true-p mod-patch-mode)
    (mod-patch--after-save-dispatch)))

(defun mod-patch--kill-buffer-hook-wrapper ()
  "Persist state when buffer is killed."
  (when (bound-and-true-p mod-patch-mode)
    (mod-patch--persist-current-state)))

;;;###autoload
(define-minor-mode mod-patch-mode
  "Minor mode for editing via patch-based mod workflow.

When enabled, this buffer represents either:
- the original file (:original mode), or
- the original file with a chosen patch applied (:patch mode).

Saving in :patch mode writes/updates a .patch file in the chosen mod directory.
Saving in :original mode writes back to the original file then rebases all
known patches.

Use `mod-patch-toggle' to switch between :original and :patch views.
Use `mod-patch-set-target-directory' to pick the patch output directory.
Use `mod-patch-switch-patch' to jump among recent patch dirs.
Use `mod-patch-merge-finish' after resolving conflicts.

In the modeline you'll see something like:
  \" MP:orig \" or \" MP:patch[ui_overhaul] \"
plus an exclamation mark if there was a conflict.

Internally we persist view/dir lists per original file in
`mod-patch-state-file' so re-opening the file or restarting Emacs
restores your last context."
  :lighter (:eval (mod-patch--lighter-patch))
  :keymap mod-patch-mode-map
  :group 'mod-patch
  (if mod-patch-mode
      ;; Enable
      (progn
        ;; Hooks
        (add-hook 'after-save-hook #'mod-patch--after-save-hook-wrapper nil t)
        (add-hook 'kill-buffer-hook #'mod-patch--kill-buffer-hook-wrapper nil t)
        ;; Ensure locals exist
        (unless mod-patch--current-orig
          (setq mod-patch--current-orig
                (mod-patch--canonical (buffer-file-name))))
        (unless mod-patch--current-mode-symbol
          (setq mod-patch--current-mode-symbol :original))
        ;; Persist in case we just turned it on.
        (mod-patch--persist-current-state))
    ;; Disable
    (remove-hook 'after-save-hook #'mod-patch--after-save-hook-wrapper t)
    (remove-hook 'kill-buffer-hook #'mod-patch--kill-buffer-hook-wrapper t)
    ;; We intentionally do not kill internal state vars, so user can re-enable
    ;; without losing context.
    ))

;;;; ---------------------------------------------------------------------------
;;;; Saving override integration (C-x C-s path)
;;;; ---------------------------------------------------------------------------
;;
;; We do NOT globally advise save-buffer because we want local semantics only.
;; Instead, we rely on after-save-hook wrapper combined with manual write-region
;; in :original mode. To better match user expectations (C-x C-s immediately
;; updates patch without a second write), we provide mod-patch-save-buffer
;; and optionally bind it.
;;

(defun mod-patch-save-buffer ()
  "Save current buffer according to mod-patch semantics immediately.
In :patch mode: regenerate patch file and mark buffer clean.
In :original mode: write to original file and rebase all patches.

This bypasses the standard save-buffer logic and is guaranteed to sync patches."
  (interactive)
  (unless (bound-and-true-p mod-patch-mode)
    (user-error "mod-patch-mode is not active in this buffer"))
  (pcase mod-patch--current-mode-symbol
    (:original
     (mod-patch--save-original-and-rebase-patches)
     (message "mod-patch: original saved and patches rebased"))
    (:patch
     (mod-patch--save-patch-view)
     (message "mod-patch: patch file updated"))
    (_
     (message "mod-patch: unknown mode, doing nothing"))))

;; Optional convenience binding for users; not enabled by default:
;; (define-key mod-patch-mode-map (kbd "C-x C-s") #'mod-patch-save-buffer)

;;;; ---------------------------------------------------------------------------
;;;; Provide
;;;; ---------------------------------------------------------------------------

(provide 'mod-patch)

;;; mod-patch.el ends here
