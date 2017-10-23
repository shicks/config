(require 'git-diff-mode)

(defun git-add-file (file)
  (interactive "F")
  (shell-command (concat "git add " file)))

(defun git-add ()
  (interactive)
  (git-add-file (buffer-file-name)))

(defun git-rm-file (file)
  (interactive "F")
  (shell-command (concat "git rm " file)))

(defun git-checkout (branch file)
  (interactive "sBranch: \nF")
  (shell-command (concat "git checkout " branch " -- " file)))

(defun git-rm ()
  (interactive)
  (git-rm-file (buffer-file-name)))

(defun save-git-add-and-next-error ()
  (interactive)
  (save-buffer) (git-add) (next-error))

(defun git-open-file-from-branch (branch)
  (interactive "sBranch: ")
  (let* ((file (buffer-file-name))
         (root (git-diff-get-root-dir))
         (tmp-file (concat "/tmp" file))
         (repo-file (replace-regexp-in-string (concat root "/") "" file))
         (source (concat branch ":" repo-file)))
    (make-directory (file-name-directory tmp-file) t)
    (shell-command (concat "git show " source " > " tmp-file))
    (find-file tmp-file)
    (toggle-read-only t))) ; autodelete to clean up?

(defun git-diff-file-against-branch (branch)
  (interactive "sBranch: ")
  (let* ((file (buffer-file-name))
         (root (git-diff-get-root-dir))
         (tmp-file (concat "/tmp" file))
         (repo-file (replace-regexp-in-string (concat root "/") "" file))
         (source (concat branch ":" repo-file)))
    (make-directory (file-name-directory tmp-file) t)
    (shell-command (concat "git show " source " > " tmp-file))
    (find-file tmp-file)
    (toggle-read-only t)
    (ediff-files tmp-file file)))

(defun git-conflicts () (interactive) (compile "git conflicts"))
(defun git-lint () (interactive) (compile "git5 lint"))

; <C-c g> is prefix keymap for git commands
(global-set-key (kbd "C-c g SPC") 'git-add)
(global-set-key (kbd "C-c g a") 'git-add)
(global-set-key (kbd "C-c g r") 'git-rm)
(global-set-key (kbd "C-c <down>") 'save-git-add-and-next-error)

(global-set-key (kbd "C-c g f") 'git-open-file-from-branch)
(global-set-key (kbd "C-c g d") 'git-diff)
(global-set-key (kbd "C-c g C-d") 'git-diff-file-against-branch)
(global-set-key (kbd "C-c g c") 'git-conflicts)
(global-set-key (kbd "C-c g l") 'git-lint)


;(add-to-list 'load-path "~/local/opt/emacs/egg")
;(require 'egg)
;(global-set-key (kbd "C-c g x") 'egg-commit-log-edit)



;(defun git-conflicts ()
;  (interactive)

; TODO(sdh): would be nice if this could be treated as a compile, so that C-x` would work...
;(defun git-next-conflict ()
;  (interactive)
;  (with
;  (shell-command (concat


(provide 'sdh-git)
