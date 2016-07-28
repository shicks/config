;; My first attempt at a major mode for recursive diffs
;; Basic idea: call 'git diff --numstat' to get a full diff
;;  - note: this does not distinguish creation from addition :-/
;; We can run git diff --summary --find-renames --find-copies [--find-copies-harder]
;;   and then annotate the renames/copies so that we can diff against them properly.


;; See hide-region.el for hints on using overlays to hide.


;; plan: --numstat and --summary - go thru numstat first, assume copies
;; then go in and amend it with summary

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-keep-variants nil)

(defface git-diff-create
  '((((class color) (background dark)) (:foreground "green"))
    (((class color) (background light)) (:background "green"))
    (t (:bold t)))
  "Face used for marking added files."
  :group 'git-diff)

(defface git-diff-delete
  '((((class color) (background dark)) (:foreground "red"))
    (((class color) (background light)) (:background "red"))
    (t (:bold t)))
  "Face used for marking deleted files."
  :group 'git-diff)

(defface git-diff-change
  '((((class color) (background dark)) (:foreground "khaki"))
    (((class color) (background light)) (:background "khaki"))
    (t (:bold t)))
  "Face used for marking changed files."
  :group 'git-diff)

(defface git-diff-copy
  '((((class color) (background dark)) (:foreground "green yellow"))
    (((class color) (background light)) (:background "green yellow"))
    (t (:bold t)))
  "Face used for marking copied files."
  :group 'git-diff)

(defface git-diff-rename
  '((((class color) (background dark)) (:foreground "orange"))
    (((class color) (background light)) (:background "orange"))
    (t (:bold t)))
  "Face used for marking renamed files."
  :group 'git-diff)

(make-variable-buffer-local
 (defvar git-diff-against nil
   "The branch being diffed against."))

(make-variable-buffer-local
 (defvar git-diff-git-root nil
   "The root directory of the current repo."))

(make-variable-buffer-local
 (defvar git-diff-files nil
   "A hash from line numbers to file stat data."))

(make-variable-buffer-local
 (defvar git-diff-dirs nil
   "A hash from line numbers to directory data."))

(defvar git-diff-window nil
  "The window currently displaying the git-diff buffer.")

(defun git-diff-get-target (file)
  "Returns the target file for a move/copy, or the original file name"
  (if (string-match "{[^{}=[:space:]]* => \\([^{}[:space:]]*\\)}" file)
      (replace-match (match-string 1 file) nil nil file)
    file))

(defun git-diff-get-source (file)
  "Returns the target file for a move/copy, or the original file name"
  (if (string-match "{\\([^{}=[:space:]]*\\) => [^{}[:space:]]*}" file)
      (replace-match (match-string 1 file) nil nil file)
    file))

(defun git-diff-change-file (change)
  (car change))
(defun git-diff-change-added (change)
  (cadr change))
(defun git-diff-change-deleted (change)
  (caddr change))
(defun git-diff-change-type (change)
  (cadddr change))
(defun git-diff-change-source (change)
  (cadddr (cdr change)))


;; data structure for hierarchy:
;; (("com"
;;    ("example"
;;      ("Foo.java" ('leaf 2 5 "change" "com/example/Foo.java"))
;;      ("common"
;;        ("Bar.java" 10 0 "create" "com/example/common/Bar.java")
;;    ...)))
;;  ("org"
;;     ...))
(defun git-diff-trie-put (key value trie)
  "Adds the entry to the directory trie and returns the modified trie.
The key should be a list of strings."
  (cond
   ;; case 1: updating an existing value
   ((and trie
         (eq 'leaf (caar trie))
         (not key))
    (cons (cons 'leaf value) (cdr trie)))
   ;; case 2: adding a value at the root
   ((not key)
    (cons (cons 'leaf value) trie))
   ;; case 3: adding additional layers
   ((not trie)
    (list (cons (car key) (git-diff-trie-put (cdr key) value '()))))
   ;; case 4: skip earlier keys (insertion sort)
   ((or (eq 'leaf (caar trie))
        (string< (caar trie) (car key)))
    (cons (car trie) (git-diff-trie-put key value (cdr trie))))
   ;; case 5: dive deeper
   ((string= (car key) (caar trie))
    (cons (cons (car key) (git-diff-trie-put (cdr key) value (cdar trie)))
          (cdr trie)))
   ;; default: insert before next key in trie
   (t
    (cons (car (git-diff-trie-put key value '())) trie))))

(defun git-diff-trie-get (key trie)
  "Returns the value, or nil if not present."
  (cond
   ;; case 1: trie is empty
   ((not trie) nil)
   ;; case 2: key is empty, look for leaf at root
   ((not key)
    (and (eq 'leaf (caar trie)) (cdar trie)))
   ;; case 3: skip earlier keys
   ((or (eq 'leaf (caar trie))
        (string< (caar trie) (car key)))
    (git-diff-trie-get key (cdr trie)))
   ;; case 4: dive deeper
   ((string= (car key) (caar trie))
    (git-diff-trie-get (cdr key) (cdar trie)))
   ;; case 5: not found
   (t nil)))

(defun git-diff-repeat (str n)
  (let ((retval ""))
    (dotimes (i n)
      (setq retval (concat retval str)))
    retval))

(defun git-diff-print-file (changes depth)
  "Prints a single file line and updates the map.
Returns t if the line is indeed a file."
  (if (and (listp (cdar changes)) (eq 'leaf (caadar changes)))
      (let* ((file (caar changes))
             (change (cdadar changes))
             (type (git-diff-change-type change))
             (face (intern-soft (concat "git-diff-" type)))
             (added (git-diff-change-added change))
             (deleted (git-diff-change-deleted change))
             )
        ;; Add the filename
        (insert (git-diff-repeat " " depth))
        (insert (propertize file 'face face))
        ;; Add the change stats
        ;(if (member face '(git-diff-rename git-diff-copy git-diff-change))
            (progn
              ; TODO(sdh): for some reason "mode change 100755 => 100644 path/to/file"
              ; lines are breaking this by making added/deleted be nil.  For now we
              ; just bypass these lines, but better would be to handle them correctly
              ; and display the change in the diff view.
              (if (and (numberp added) (> added 0))
                  (insert (propertize (concat "  +" (number-to-string added))
                                      'face 'git-diff-create)))
              (if (and (numberp deleted) (> deleted 0))
                  (insert (propertize (concat "  -" (number-to-string deleted))
                                      'face 'git-diff-delete)))
              )
        ;)
        ;; Record the file in the hash
        (puthash (line-number-at-pos) change git-diff-files)
        (insert "\n")
        t)))

;; Input:
;;  (("com" ("google" ...) ...) ...)
;;  (("File" ('leaf ...)) ...?)
;; we need to keep track of several things by line...
;;  - dirs = hash from line number of the folder to end of folder
;;  - files = hash from line number to (source . target)
(defun git-diff-print-tree (changes depth)
  "Iterates through the trie and prints out the directory tree."
  (cond
   ;; quit at end
   ((not changes) nil)
   ;; print a leaf
   ;; TODO(sdh): add stats, colors, record in hashes
   ((git-diff-print-file changes depth)
    (git-diff-print-tree (cdr changes) depth))
   ;; print a directory
   ;; TODO(sdh): add cumulative stats, record in hashes
   (t
    (let ((dir-line (line-number-at-pos))
          (dir-line-end))
      (insert (concat (git-diff-repeat " " depth) (caar changes) "\n"))
      (setq dir-line-end (- (point) 1))
      (git-diff-print-tree (cdar changes) (+ 2 depth))
      (puthash dir-line (list dir-line-end (- (point) 1)) git-diff-dirs))
    (git-diff-print-tree (cdr changes) depth))))

;; data structure for a change
;; ((split file name) added-lines deleted-lines type source)
;; split file name is result of (split-string name "/" t)
;; deleted-lines and added-lines are numers
;; type is either "change", "create", "delete", "rename", or "copy"
;; source is another file, only in case of "rename" or "copy"

;; We use a lexical sort for the file names.
;; For renames, we add a second entry for the delete, which may
;; be out of order.
;; We show add/move/change/delete/copy in all different colors...

;(setq debug-on-error t)

(defun git-diff-parse ()
  "Iterates through the numstat and summary outputs and returns
a list of changes, using the structure specified above."
  (setq git-diff-files (make-hash-table))
  (setq git-diff-dirs (make-hash-table))
  (let ((changes '())
        (re1 "^\\([-0-9]*\\)\t\\([-0-9]*\\)\t\\([^\n]*\\)")
        (re2 (concat
             "^ \\([a-z]+\\) "           ; $1: create|delete|rename|copy
             "\\(mode [0-9]\\{6\\} \\)?" ; $2: mode missing in rename/copy
             "\\([^{[:space:]]*"         ; $3: arbitrary non-braces
             "\\({[^ ]* => [^}]*}"       ;     possible {foo => bar}
             "[^([:space:]]*\\)?\\)"))   ;     end of filename, skip "(NN%)"
        (cur 0)  ; current point in buffer

        )
    (beginning-of-buffer)
    ;; TODO(sdh): iterate backwards is an order of magnitude more efficient
    (goto-char (or (re-search-forward re2 (point-max) t) (point-max)))
    (while (progn (setq cur (re-search-backward re1 (point-min) t)) cur)
      (let* ((added (string-to-number (match-string 1)))
             (deleted (string-to-number (match-string 2)))
             (file (match-string 3))
             (target (git-diff-get-target file))
             (key (split-string target "/" t))
             (source (git-diff-get-source file)))
        (setq changes
              (git-diff-trie-put key
                                 (list target added deleted "change" source)
                                 changes))
        (goto-char cur)))
    (goto-char (point-max))
    (while (progn (setq cur (re-search-backward re2 (point-min) t)) cur)
      (let* ((type (match-string 1))
             (file (match-string 3))
             (target (git-diff-get-target file))
             (key (split-string target "/" t))
             (source (git-diff-get-source file))
             (source-key (split-string source "/" t))
             (change (git-diff-trie-get key changes))
             (added (git-diff-change-added change))
             (deleted (git-diff-change-deleted change)))
        (setq changes
              (git-diff-trie-put key
                                 (list target added deleted type source)
                                 changes))
        (if (string= type "rename")
            (progn
            (setq changes
                  (git-diff-trie-put source-key
                                     (list source 0 0 "delete" source)
                                     changes))
            ))
        (goto-char cur)))
    ;; now that everything's loaded, delete buffer and write our own structure
    (erase-buffer)
    (remove-overlays)
    (git-diff-print-tree changes 0)
))

(defun git-diff-read-changes (table)
  "Iterates through the summary output and stores the change type
in a hash table."
  (let ((summary-start (re-search-forward "^ " (buffer-size) t))
        (re (concat
             "^ \\([a-z]*\\) "           ; $1: create|delete|rename|copy
             "\\(mode [0-9]\\{6\\} \\)?" ; $2: mode missing in rename/copy
             "\\([^{[:space:]]*"         ; $3: arbitrary non-braces
             "\\({[^ ]* => [^}]*}"       ; possible {foo => bar}
             "[^([:space:]]*\\)?\\)"))   ; end of filename
        (found t))
    (while found
      (setq found (re-search-forward re (buffer-size) t))
      (if found (puthash (match-string 3) (match-string 1) table)))
    (delete-region summary-start (buffer-size))
))

(defun git-diff (branch)
  "Runs git diff against the given branch and presents
a buffer of recursive directory/file diffs, linking to ediff
to change individual files."
  (interactive "sBranch: ")
  (setq debug-on-error t)
  (let ((pwd default-directory)
        (buf (get-buffer-create "*git-diff*"))
        (cmd (concat "git diff --numstat --summary "
                     "--find-renames --find-copies "
                     branch)))
    (switch-to-buffer buf)
    (setq default-directory pwd)
    (shell-command cmd buf)
    (git-diff-mode)
    (toggle-read-only 0)
    (git-diff-parse)
    (toggle-read-only 1)
    (setq git-diff-against branch)
    (setq git-diff-git-root nil)
    (goto-char 0)
))

(defun git-diff-get-root-dir ()
  "Computes the root dir of the current directory repo"
  (or git-diff-git-root (setq git-diff-git-root
                              (git-diff-dirname (shell-command-to-string
                                                 "git rev-parse --git-dir")))))

;; TODO(sdh): set up more keybindings, possibly with
;; better/more automatic ediff integration, back and forth, etc

(defun git-diff-dirname (name)
  "Returns the dirname."
  (replace-regexp-in-string "/[^/]*$" "" name))

(defun git-diff-open-diff (pos)
  "Starts ediff mode for the given change"
  (interactive "d")
  (let* ((change (gethash (line-number-at-pos pos) git-diff-files))
         (root (git-diff-get-root-dir))
         (rhs-file (concat (file-name-as-directory root)
                           (git-diff-change-file change)))
         (lhs-file (concat "/tmp"
                           (file-name-as-directory root)
                           (git-diff-change-source change)))
         (source-rev (concat git-diff-against ":"
                             (git-diff-change-source change)))
         ;(rhs (find-file rhs-file))
         (lhs (progn
                (make-directory (git-diff-dirname lhs-file) t)
                (shell-command (concat "git show " source-rev " > " lhs-file))
                ;(find-file lhs-file)
                ))
         )
    ;(shell-command (concat "git show " source-rev) lhs)
    ;(with-current-buffer lhs
      ;(set-visited-file-name (concat "/tmp" rhs-file))
      ;(not-modified)
      ;(eval (list (intern-soft (with-current-buffer rhs major-mode)))))
    (setq git-diff-window (frame-selected-window))
    (with-current-buffer (ediff-files lhs-file rhs-file)
      (ediff-update-diffs)
      (ediff-toggle-read-only ediff-buffer-A))
    ;; TODO(sdh): add a clean-up hook to delete the temp file
    ;; also look into a quit hook to set current buffer back to *git-diff*
    ;; might also update the *git-diff* buffer at end with new diff info?
))

(defun git-diff-toggle-dir-visibility (pos)
  "Starts ediff mode for the given change"
  (interactive "d")
  (let* ((line (line-number-at-pos pos))
         (dir (gethash line git-diff-dirs))
         (start (car dir))
         (end (cadr dir))
         (has-overlay (cddr dir))
         (overlay (if has-overlay (car has-overlay)
               (make-overlay start end))))
    (if has-overlay
        (progn
          (puthash line (list start end) git-diff-dirs)
          (delete-overlay overlay))
      (puthash line (list start end overlay) git-diff-dirs)
      (overlay-put overlay 'invisible t)
      (overlay-put overlay 'intangible t)
      (overlay-put overlay 'before-string "..."))
))

(defun git-diff-act-on-line (pos)
  "Open a diff or toggle directory visibility"
  (interactive "d")
  (cond
   ((gethash (line-number-at-pos pos) git-diff-files)
    (git-diff-open-diff pos))
   ((gethash (line-number-at-pos pos) git-diff-dirs)
    (git-diff-toggle-dir-visibility pos))
   (t (message "Not on a file or directory"))))

(defun git-diff-revert-deleted-file (pos)
  "Reverts a delete by copying the file into the working directory."
  (interactive "d")
  (let* ((line (line-number-at-pos pos))
         (change (gethash line git-diff-files))
         (aschange (list (git-diff-change-file change)
                         0 0 "change"
                         (git-diff-change-source change)))
         (root (or git-diff-git-root (git-diff-get-root-dir)))
         (rhs-file (concat (file-name-as-directory root)
                           (git-diff-change-file change)))
         (source-rev (concat git-diff-against ":"
                             (git-diff-change-source change)))
         ;; changes current buffer
         (rhs (progn
                (if (not (file-writable-p rhs-file))
                    (make-directory (file-name-directory rhs-file) t))
                (find-file rhs-file)))
         )
    (with-current-buffer "*git-diff*"
      (puthash line aschange git-diff-files)
      (save-excursion
        (goto-char pos)
        (toggle-read-only nil)
        (remove-text-properties (point-at-bol) (point-at-eol) '(face nil))
        (toggle-read-only t)))
    (shell-command (concat "git show " source-rev) rhs)
))

(defun git-diff-revert-added-file (pos)
  "Reverts an add by deleting the file from the working directory."
  (interactive "d")
  (let* ((line (line-number-at-pos pos))
         (change (gethash line git-diff-files))
         (root (or git-diff-git-root (git-diff-get-root-dir)))
         (rhs-file (concat (file-name-as-directory root)
                           (git-diff-change-file change)))
         )
    (if (string= (git-diff-change-type change) "create") nil
      (error "Can only delete adds"))
    (save-excursion
      (goto-char pos)
      (toggle-read-only nil)
      (add-text-properties (point-at-bol) (+ 1 (point-at-eol))
                           '(invisible t intangible t))
      (toggle-read-only t))
    (delete-file rhs-file)
))

(defun git-diff-next-diff ()
  (interactive) ; TODO(sdh): prefix/count argument?
  (next-line) ; TODO(sdh): make this skip dirs
)
(defun git-diff-prev-diff ()
  (interactive) ; TODO(sdh): prefix/count argument?
  (previous-line) ; TODO(sdh): make this skip dirs
)
; TODO(sdh): UPDATE DIFF AS THINGS CHANGE!
; TODO(sdh): automatically create dir in revert-deleted-file


(defun git-diff-ediff-quit-action ()
  (if git-diff-window (progn
    (select-window git-diff-window)
    (setq git-diff-window nil))))

(define-derived-mode git-diff-mode
  special-mode "Git Diff"
  "Major mode for directory-recursive git diffs.
\\{git-diff-mode-map}"
)

; We need to append so it comes after ediff-cleanup-mess
(if (boundp 'ediff-quit-hook) (progn
  (nconc ediff-quit-hook '(git-diff-ediff-quit-action))
  (remove-duplicates ediff-quit-hook)
))

(define-key git-diff-mode-map (kbd "RET") 'git-diff-act-on-line)
(define-key git-diff-mode-map (kbd "x") 'git-diff-toggle-dir-visibility)
(define-key git-diff-mode-map (kbd "d") 'git-diff-open-diff)
(define-key git-diff-mode-map (kbd "a") 'git-diff-revert-deleted-file)
(define-key git-diff-mode-map (kbd "r") 'git-diff-revert-added-file)
(define-key git-diff-mode-map (kbd "n") 'git-diff-next-diff)
(define-key git-diff-mode-map (kbd "p") 'git-diff-prev-diff)

(provide 'git-diff-mode)
