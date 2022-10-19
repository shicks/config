;; My first attempt at a major mode for recursive diffs
;; Basic idea: call 'git diff --numstat' to get a full diff
;;  - note: this does not distinguish creation from addition :-/
;; We can run git diff --summary --find-renames --find-copies [--find-copies-harder]
;;   and then annotate the renames/copies so that we can diff against them properly.


;; See hide-region.el for hints on using overlays to hide.


;; plan: --numstat and --summary - go thru numstat first, assume copies
;; then go in and amend it with summary

(require 'cl-lib)

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

(defconst git-diff-helper
  (concat (file-name-directory load-file-name) "diff-mode-helper")
  "Helper script to get diffs across repo types.")

(make-variable-buffer-local
 (defvar git-diff-against nil
   "The branch being diffed against."))

(make-variable-buffer-local
 (defvar git-diff-root nil
   "The root directory of the current repo."))

(make-variable-buffer-local
 (defvar git-diff-files nil
   "A hash from line numbers to file stat data."))

(make-variable-buffer-local
 (defvar git-diff-dirs nil
   "A hash from line numbers to directory data."))

(defvar git-diff-window nil
  "The window currently displaying the git-diff buffer.")

(make-variable-buffer-local
 (defvar git-diff-vcs 'git
   "Which type of VCS is being used ('git, 'hg, or 'p4)."))

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
  (cl-caddr change))
(defun git-diff-change-type (change)
  (cl-cadddr change))
(defun git-diff-change-source (change)
  (cl-cadddr (cdr change)))

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
  (if (and (listp (cdar changes)) (eq 'leaf (cl-caadar changes)))
      (let* ((file (caar changes))
             (change (cl-cdadar changes))
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

(defconst git-diff-re
  ;; (re1 "^\\([-0-9]*\\)\t\\([-0-9]*\\)\t\\([^\n]*\\)")
  (rx line-start
      (group-n 1 (or "create" "delete" "change")) (+ space)  ; $mode
      (group-n 2 (+ digit)) (+ space)                        ; $added
      (group-n 3 (+ digit)) (+ space)                        ; $removed
      (group-n 4 (+ not-newline)))                           ; $filename
  "Regex for 'diff-helper' lines, e.g. 'change   19    2 path/to/file'")

(defun git-diff-parse ()
  "Iterates through the numstat and summary outputs and returns
a list of changes, using the structure specified above."
  (setq git-diff-files (make-hash-table))
  (setq git-diff-dirs (make-hash-table))
  (let ((changes '())
        (cur 0))  ; current point in buffer
    (beginning-of-buffer)
    ;; TODO(sdh): iterate backwards is an order of magnitude more efficient
    (goto-char (point-max))
    (while (progn (setq cur (re-search-backward git-diff-re (point-min) t)) cur)
      (let* ((mode (match-string 1))
             (added (string-to-number (match-string 2)))
             (deleted (string-to-number (match-string 3)))
             (file (match-string 4))
             (key (split-string file "/" t)))
        (message "Line: %s %s %s %s" mode file added deleted)
        (setq changes
              (git-diff-trie-put key
                                 (list file added deleted mode file)
                                 changes))
        (goto-char cur)))
    (goto-char (point-max))
    ;; now that everything's loaded, delete buffer and write our own structure
    (erase-buffer)
    (remove-overlays)
    (git-diff-print-tree changes 0)
))

(defun git-diff-read-changes (table)
  "Iterates through the summary output and stores the change type
in a hash table."
  (let ((summary-start (re-search-forward "^ " (buffer-size) t))
        (found t))
    (while found
      (setq found (re-search-forward git-diff-re-numstat-special (buffer-size) t))
      (if found (puthash (match-string 2) (match-string 1) table)))
    (delete-region summary-start (buffer-size))
))


(defun git-diff (branch)
  "Runs git diff against the given branch and presents
a buffer of recursive directory/file diffs, linking to ediff
to change individual files."
  (interactive "sBranch: ")
  (git-diff-internal branch 'git))

(defun git-diff-hg (rev)
  "Version of git-diff for Hg."
  (interactive "sRev: ")
  (git-diff-internal rev 'hg))

(defun git-diff-p4 ()
  "Version of git-diff for P4."
  (interactive)
  (git-diff-internal "" 'p4))

(defun git-diff-internal (against vcs)
  (setq debug-on-error t)
  (let ((pwd default-directory)
        (buf (get-buffer-create "*git-diff*"))
        (cmd (concat git-diff-helper " " against)))
    ;(message (format "shell command: %s" cmd))
    (switch-to-buffer buf)
    (setq default-directory pwd)
    (shell-command cmd buf)
    (git-diff-mode)
    (toggle-read-only 0)
    (git-diff-parse)
    (toggle-read-only 1)
    (setq git-diff-against against)
    (setq git-diff-root nil)
    (setq git-diff-vcs vcs)
    (goto-char 0)
    ;; Prevent ediff from opening new frames!
    (defun ediff-window-display-p () nil)
))

(defun git-diff-get-root-dir ()
  "Computes the root dir of the current directory repo"
  (or git-diff-root (setq git-diff-root
                          (replace-regexp-in-string "\n$" ""
                           (shell-command-to-string
                            (cond
                             ((eq git-diff-vcs 'hg) "hg root")
                             ((eq git-diff-vcs 'git) "git rev-parse --show-toplevel")
                             ((eq git-diff-vcs 'p4) "p4 root" "p4 info | sed -n 's/Client root: //p'")))))))

;; TODO(sdh): set up more keybindings, possibly with
;; better/more automatic ediff integration, back and forth, etc

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
         (command-git (concat "git show " git-diff-against ":"
                              (git-diff-change-source change)))
         (command-hg (concat "hg cat --hidden -r " git-diff-against " " root "/"
                             (git-diff-change-source change)))
         (command-p4 (concat "p4 cat " root "/" (git-diff-change-source change)))
         (command (cond
                   ((eq git-diff-vcs 'hg) command-hg)
                   ((eq git-diff-vcs 'git) command-git)
                   ((eq git-diff-vcs 'p4) command-p4)))
         ;(rhs (find-file rhs-file))
         (lhs (progn
                (message "Open: [%s] %s %s" command lhs-file rhs-file)
                (make-directory (file-name-directory lhs-file) t)
                (shell-command (concat command " > " lhs-file))
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
         (root (or git-diff-root (git-diff-get-root-dir)))
         (rhs-file (concat (file-name-as-directory root)
                           (git-diff-change-file change)))
         (command-git (concat "git show " git-diff-against ":"
                              (git-diff-change-source change)))
         (command-hg (concat "hg cat --hidden -r " git-diff-against " " root "/"
                             (git-diff-change-source change)))
         (command-p4 (concat "p4 cat " root "/" (git-diff-change-source change)))
         (command (cond
                   ((eq git-diff-vcs 'hg) command-hg)
                   ((eq git-diff-vcs 'git) command-git)
                   ((eq git-diff-vcs 'p4) command-p4)))
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
    (shell-command command rhs)
))

(defun git-diff-revert-added-file (pos)
  "Reverts an add by deleting the file from the working directory."
  (interactive "d")
  (let* ((line (line-number-at-pos pos))
         (change (gethash line git-diff-files))
         (root (or git-diff-root (git-diff-get-root-dir)))
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
(define-key git-diff-mode-map (kbd "j") 'git-diff-next-diff)
(define-key git-diff-mode-map (kbd "k") 'git-diff-prev-diff)

(provide 'git-diff-mode)
