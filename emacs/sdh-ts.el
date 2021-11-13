;;; https://searchcode.com/file/107184872/flycheck-typescript.el

(require 'flycheck)
(require 'lsp-mode)
;(require 'lsp-typescript)
(require 'yasnippet)

(setq sdh-ts-sort-imports-file
      (concat (file-name-directory load-file-name) "/ts-sort-imports"))

(defun sdh-dirname (arg)
  "Behaves equivalently to unix `dirname`: ignores and removes trailing slash"
  (directory-file-name (file-name-directory (directory-file-name arg))))

(defvar sdh-ts-root nil
  "Path to the tsconfig.json file, if found")
(make-variable-buffer-local 'sdh-ts-root)
(defun sdh-ts-find-root ()
  (unless sdh-ts-root
    (let ((dir (sdh-dirname file-name-directory)))
      (while (not (string= dir "/"))
        (when (file-exists-p (concat dir "/tsconfig.json"))
          (setq 
                  
        ; check...
        (setq dir (sdh-dirname dir))))
                                      
)
  
      (file-name-directory (directory-file-name (file-name-directory load-file-name))))

(setq exec-path (cons (concat (getenv "HOME") "/local/bin") exec-path))


(defun sdh-ts-sort-imports ()
  (interactive)
  (let ((start (save-excursion (beginning-of-buffer)
                               (re-search-forward "^import ")
                               (beginning-of-line)
                               (point)))
        (end (save-excursion (end-of-buffer)
                             (re-search-backward "^import ")
                             (beginning-of-line)
                             (next-line)
                             (point))))
    (save-excursion
      (shell-command-on-region start end
                               (concat sdh-ts-root "/scripts/sort-imports")
                               nil t))))

;;; NOTE: UNCOMMENT AND RUN THIS LINE???
;;;       May also need to run 'npm i -g typescript-language-server'
;;;       and/or add nvm's bin directory to the path
;;;       The variable 'exec-path' is also relevant???
;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection (concat sdh-ts-root "node_modules/typescript-language-server/lib/cli.js"))
;;                   :major-modes '(typescript-mode)
;;                   :server-id 'ts))

;; (add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable) ;; for typescript support

;(lsp-register-client
; (make-lsp-client :new-connection (

(flycheck-define-checker typescript-tsc
  "A TypeScript syntax checker using tsc, the TypeScript compiler.
See URL `http://www.typescriptlang.org/'."
  :command ("flycheck-tsc" source-original)
  :working-directory (lambda (x) sdh-ts-root)
  :error-patterns
  ((error line-start (file-name) "(" line "," column "): error"
          (message (one-or-more not-newline)
                   (zero-or-more "\n\t" (one-or-more not-newline)))
          line-end))
  :modes typescript-mode
  ;; :next-checkers ((warnings-only . typescript-tslint))
)

;; 2020/02/10: The lint checks are just annoying...
;; (flycheck-define-checker typescript-tslint
;;   "A TypeScript style checker using tslint.

;; See URL `https://github.com/palantir/tslint'."
;;   :command ("tslint"
;;             ;"--project" "tsconfig.json"
;;             "--config" "tslint.json"
;;             ;source-inplace)
;;             source)
;;   :working-directory (lambda (x) sdh-ts-root)
;;   :error-patterns ((warning "ERROR: " (file-name) ":" line ":" column " - " (message)))
;;   :modes typescript-mode)
;; ;(flycheck-add-next-checker 'lsp-ui 'typescript-tslint)

;; (flycheck-define-generic-checker 'lsp-ts
;;   "A syntax checker using the Language Server Protocol (RLS)
;; provided by lsp-mode.

;; See https://github.com/emacs-lsp/lsp-mode."
;;   :start #'lsp-ui-flycheck--start
;;   :modes '(python-mode) ; Need a default mode
;;   :predicate (lambda () lsp-mode)
;;   :error-explainer (lambda (e) (flycheck-error-message e))
;;   :next-checkers ((warnings-only . typescript-tslint))
;; )

;; (flycheck-def-config-file-var flycheck-tslint.json typescript-tslint "tslint.json")

;; Link to the correct checker script...?
(setq flycheck-typescript-tsc-executable (concat (file-name-directory load-file-name) "flycheck-tsc"))
;; (setq flycheck-typescript-tslint-executable (concat sdh-ts-root "node_modules/tslint/bin/tslint"))
;(setq flycheck-typescript-tsc-executable (concat (file-name-directory load-file-name) "flycheck-tslint"))


(defun sdh-ts-init ()
  ; TODO - tide seems to be clobbering flycheck, and unneeded for company...
  (tide-setup)
  (tide-mode)
  (company-mode t)
  (flycheck-mode)
  ;(lsp)
  (setq lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-braces nil)
  (define-key typescript-mode-map (kbd "C-c C-e") 'flycheck-display-error-at-point)
  (define-key typescript-mode-map (kbd "C-c j i") 'sdh-ts-sort-imports)
  ;; Note: these default to 'typescript-insert-and-indent, which is annoying
  (define-key typescript-mode-map (kbd ",") 'self-insert-command)
  (define-key typescript-mode-map (kbd ";") 'self-insert-command)
  (define-key typescript-mode-map (kbd ":") 'self-insert-command)
  (define-key typescript-mode-map (kbd ")") 'self-insert-command)
  (define-key typescript-mode-map (kbd "(") 'self-insert-command)
  (define-key typescript-mode-map (kbd "}") 'self-insert-command)
  (define-key typescript-mode-map (kbd "{") 'self-insert-command)
  (define-key typescript-mode-map (kbd "M-RET") 'company-complete)
  (define-key typescript-mode-map (kbd "C-'") 'company-complete)
  ;; https://github.com/emacs-lsp/lsp-ui/issues/266
  ;(remove-hook 'lsp-after-diagnostics-hook 'lsp-ui-sideline--diagnostics-changed t)
  ; For some reason I can't actually just disable it straightaway.
  (define-key typescript-mode-map (kbd "C-c \\") 'lsp-ui-sideline-mode)
)

;(set-variable 'lsp-ui-sideline-enable nil)

(add-hook 'typescript-mode-hook 'sdh-ts-init)

(add-to-list 'flycheck-checkers 'typescript-tsc)
;(add-to-list 'flycheck-checkers 'typescript-tslint)

(provide 'sdh-ts)
