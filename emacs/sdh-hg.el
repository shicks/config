(require 'git-diff-mode)

(defun sdh-hg-open-file-from-rev (rev)
  (interactive "sRev: ")
  (let* ((file (buffer-file-name))
         (tmp-file (concat "/tmp" file)))
    (make-directory (file-name-directory tmp-file) t)
    (shell-command (concat "hg cat -r " rev " " file " > " tmp-file))
    (find-file tmp-file)
    (toggle-read-only t))) ; autodelete to clean up?

(defun sdh-hg-diff-file-against-rev (rev)
  (interactive "sRev: ")
  (let* ((file (buffer-file-name))
         (tmp-file (concat "/tmp" file)))
    (make-directory (file-name-directory tmp-file) t)
    (shell-command (concat "hg cat -r " rev " " file " > " tmp-file))
    (find-file tmp-file)
    (toggle-read-only t)
    (ediff-files tmp-file file)))


(defun sdh-hg-resolve-mark-file (file)
  (interactive "F")
  (shell-command (concat "hg resolve --mark " file)))
(defun sdh-hg-resolve-mark ()
  (interactive)
  (sdh-hg-resolve-mark-file (buffer-file-name)))

(defun sdh-hg-add-file (file)
  (interactive "F")
  (shell-command (concat "hg add " file)))
(defun sdh-hg-add ()
  (interactive)
  (sdh-hg-add-file (buffer-file-name)))

(defun sdh-hg-rm-file (file)
  (interactive "F")
  (shell-command (concat "hg rm " file)))
(defun sdh-hg-rm ()
  (interactive)
  (sdh-hg-rm-file (buffer-file-name)))

(defun sdh-hg-conflicts () (interactive) (compile "hg-conflicts"))
(defun sdh-hg-lint (all)
  (interactive "P")
  (compile (if all "hg-lint -v" "hg-lint")))

; <C-c h> is prefix keymap for hg commands
(global-set-key (kbd "C-c h SPC") 'sdh-hg-resolve-mark)
(global-set-key (kbd "C-c h a") 'sdh-hg-add)
(global-set-key (kbd "C-c h r") 'sdh-hg-rm)
(global-set-key (kbd "C-c h d") 'git-diff-hg)
(global-set-key (kbd "C-c h C-d") 'sdh-hg-diff-file-against-rev)
(global-set-key (kbd "C-c h c") 'sdh-hg-conflicts)
(global-set-key (kbd "C-c h l") 'sdh-hg-lint)

(provide 'sdh-hg)
