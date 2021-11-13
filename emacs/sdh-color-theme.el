;(eval-when-compile    (require 'color-theme-modern))

; For some reason "unspecified-fg" and "unspecified-bg" don't work on my
; macbook, and both show up as black, making things very difficult to use.
; Instead, just swap them out for black and white, but keep track of where
; they were.
(defvar sdh-unspecified-bg "black")
(defvar sdh-unspecified-fg "white")

(deftheme sdh-color-theme
  "Color theme by Steve Hicks, created 2018-01-25.")

(custom-theme-set-faces
 'sdh-color-theme

 '(default ((t (:background "black" :foreground "white"))))
 
      (background-mode . dark)
      (foreground-color . sdh-unspecified-fg))
     ((apropos-match-face . match)
      (compilation-message-face . underline)
      (cperl-here-face . font-lock-string-face)
      (cperl-invalid-face . cperl-my-trailing-spaces-face)
      (cperl-pod-face . font-lock-comment-face)
      (cperl-pod-head-face . font-lock-variable-name-face)
      (fci-rule-color . "#cccccc")
      (google-prodaccess-bad-modeline-face . font-lock-warning-face)
      (google-prodaccess-close-modeline-face :foreground "orange3" :weight bold)
      (google-prodaccess-good-modeline-face :foreground "darkgreen")
      (hl-line-face . hl-line)
      (ispell-highlight-face . isearch)
      (list-matching-lines-buffer-name-face . underline)
      (list-matching-lines-face . match)
      (list-matching-lines-prefix-face . shadow)
      (ps-line-number-color . "black")
      (ps-zebra-color . 0.95)
      (tags-tag-face . default)
      (view-highlight-face . highlight)
      (widget-mouse-face . highlight))
     (default ((t (:stipple nil :background sdh-unspecified-bg :foreground sdh-unspecified-fg :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
     (Info-quoted ((t (:family "courier"))))
     (apropos-function-button ((t (:foreground "LightSkyBlue"))))
     (apropos-keybinding ((t (:underline t))))
     (apropos-misc-button ((t (:foreground "Aquamarine"))))
     (apropos-property ((t (:foreground "LightSteelBlue"))))
     (apropos-symbol ((t (:bold t :weight bold))))
     (apropos-user-option-button ((t (:foreground "LightGoldenrod"))))
     (apropos-variable-button ((t (:foreground "LightGoldenrod"))))
     (bold ((t (:bold t :weight bold))))
     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
     (border ((t (nil))))
     (buffer-menu-buffer ((t (:bold t :weight bold))))
     (button ((t (:underline t :foreground "cyan1"))))
     (c-annotation-face ((t (:foreground "Aquamarine"))))
     (c-nonbreakable-space-face ((t (:bold t :weight bold :background sdh-unspecified-fg :foreground sdh-unspecified-bg))))
     (closure-template-tag ((t (:bold t :foreground "PaleGreen" :weight bold))))
     (comint-highlight-input ((t (:bold t :weight bold))))
     (comint-highlight-prompt ((t (:foreground "cyan"))))
     (compilation-column-number ((t (:italic t :slant italic :foreground "LightSalmon"))))
     (compilation-error ((t (:bold t :weight bold :foreground "Pink"))))
     (compilation-info ((t (:bold t :weight bold :foreground "Green1"))))
     (compilation-line-number ((t (:foreground "Cyan1"))))
     (compilation-mode-line-exit ((t (:bold t :foreground "ForestGreen" :weight bold))))
     (compilation-mode-line-fail ((t (:bold t :foreground "Red1" :weight bold))))
     (compilation-mode-line-run ((t (:bold t :foreground "DarkOrange" :weight bold))))
     (compilation-warning ((t (:bold t :weight bold :foreground "DarkOrange"))))
     (completions-annotations ((t (:italic t :slant italic))))
     (completions-common-part ((t (nil))))
     (completions-first-difference ((t (:bold t :weight bold))))
     (cperl-array-face ((t (:background "wheat" :foreground "tan4" :weight normal))))
     (cperl-hash-face ((t (:italic t :bold t :background "wheat" :foreground "Red" :slant italic :weight bold))))
     (cperl-my-trailing-spaces-face ((t (:background "color-236"))))
     (cperl-nonoverridable-face ((t (:foreground "orchid1"))))
     (cursor ((t (:background "white"))))
     (custom-button ((t (nil))))
     (custom-button-mouse ((t (nil))))
     (custom-button-pressed ((t (nil))))
     (custom-button-pressed-unraised ((t (:underline t :foreground "violet"))))
     (custom-button-unraised ((t (:underline t))))
     (custom-changed ((t (:background "blue1" :foreground "white"))))
     (custom-comment ((t (:background "yellow3" :foreground "black"))))
     (custom-comment-tag ((t (:foreground "gray80"))))
     (custom-documentation ((t (nil))))
     (custom-face-tag ((t (:bold t :weight bold :foreground "light blue"))))
     (custom-group-subtitle ((t (:bold t :weight bold))))
     (custom-group-tag ((t (:bold t :family "Sans Serif" :foreground "light blue" :weight bold :height 1.2))))
     (custom-group-tag-1 ((t (:bold t :family "Sans Serif" :foreground "pink" :weight bold :height 1.2))))
     (custom-invalid ((t (:background "red1" :foreground "yellow1"))))
     (custom-link ((t (:underline t :foreground "cyan1"))))
     (custom-modified ((t (:background "blue1" :foreground "white"))))
     (custom-rogue ((t (:background "black" :foreground "pink"))))
     (custom-saved ((t (:underline t))))
     (custom-set ((t (:background "white" :foreground "blue1"))))
     (custom-state ((t (:foreground "lime green"))))
     (custom-themed ((t (:background "blue1" :foreground "white"))))
     (custom-variable-button ((t (:bold t :underline t :weight bold))))
     (custom-variable-tag ((t (:bold t :foreground "light blue" :weight bold))))
     (custom-visibility ((t (:underline t :foreground "cyan1" :height 0.8))))
     (diff-added ((t (:background "#335533"))))
     (diff-changed ((t (nil))))
     (diff-context ((t (:foreground "#dddddd"))))
     (diff-file-header ((t (:bold t :background "grey60" :weight bold))))
     (diff-function ((t (:background "grey45"))))
     (diff-header ((t (:background "grey45"))))
     (diff-hunk-header ((t (:background "grey45"))))
     (diff-index ((t (:bold t :weight bold :background "grey60"))))
     (diff-indicator-added ((t (:background "#335533"))))
     (diff-indicator-changed ((t (nil))))
     (diff-indicator-removed ((t (:background "#553333"))))
     (diff-nonexistent ((t (:bold t :weight bold :background "grey60"))))
     (diff-refine-added ((t (:background "#22aa22"))))
     (diff-refine-changed ((t (:background "#aaaa22"))))
     (diff-refine-removed ((t (:background "#aa2222"))))
     (diff-removed ((t (:background "#553333"))))
     (dired-directory ((t (:foreground "LightSkyBlue"))))
     (dired-flagged ((t (:bold t :weight bold :foreground "Pink"))))
     (dired-header ((t (:foreground "PaleGreen"))))
     (dired-ignored ((t (:foreground "grey70"))))
     (dired-mark ((t (:foreground "Aquamarine"))))
     (dired-marked ((t (:bold t :weight bold :foreground "DarkOrange"))))
     (dired-perm-write ((t (:italic t :slant italic :foreground "chocolate1"))))
     (dired-symlink ((t (:foreground "Cyan1"))))
     (dired-warning ((t (:bold t :foreground "Pink" :weight bold))))
     (ediff-current-diff-A ((t (:background "tomato" :foreground "black"))))
     (ediff-current-diff-Ancestor ((t (:background "VioletRed"))))
     (ediff-current-diff-B ((t (:background "pale green" :foreground "black"))))
     (ediff-current-diff-C ((t (:background "#888833"))))
     (ediff-even-diff-A ((t (:background "skyblue" :foreground "black"))))
     (ediff-even-diff-Ancestor ((t (:background "Grey"))))
     (ediff-even-diff-B ((t (:background "skyblue" :foreground "black"))))
     (ediff-even-diff-C ((t (:background "light grey"))))
     (ediff-fine-diff-A ((t (:background "#aa2222"))))
     (ediff-fine-diff-Ancestor ((t (:background "Green"))))
     (ediff-fine-diff-B ((t (:background "#22aa22"))))
     (ediff-fine-diff-C ((t (:background "#aaaa22"))))
     (ediff-odd-diff-A ((t (:background "plum" :foreground "black"))))
     (ediff-odd-diff-Ancestor ((t (:background "gray40"))))
     (ediff-odd-diff-B ((t (:background "plum" :foreground "black"))))
     (ediff-odd-diff-C ((t (:background "Grey"))))
     (edit-indirect-edited-region ((t (:background "SkyBlue4"))))
     (eldoc-highlight-function-argument ((t (:bold t :weight bold))))
     (error ((t (:bold t :foreground "Pink" :weight bold))))
     (escape-glyph ((t (:foreground "cyan"))))
     (ffap ((t (:background "darkolivegreen"))))
     (file-name-shadow ((t (:foreground "grey70"))))
     (fixed-pitch ((t (:family "Monospace"))))
     (flycheck-error ((t (:bold t :weight bold :foreground "Pink" :underline t))))
     (flycheck-error-list-checker-name ((t (:foreground "LightSkyBlue"))))
     (flycheck-error-list-column-number ((t (:foreground "Aquamarine"))))
     (flycheck-error-list-error ((t (:bold t :weight bold :foreground "Pink"))))
     (flycheck-error-list-highlight ((t (:background "darkolivegreen"))))
     (flycheck-error-list-id ((t (:foreground "PaleGreen"))))
     (flycheck-error-list-id-with-explainer ((t (:foreground "PaleGreen" :box (:style released-button)))))
     (flycheck-error-list-info ((t (:bold t :weight bold :foreground "Green1"))))
     (flycheck-error-list-line-number ((t (:foreground "Aquamarine"))))
     (flycheck-error-list-warning ((t (:bold t :weight bold :foreground "DarkOrange"))))
     (flycheck-fringe-error ((t (:bold t :weight bold :foreground "Pink"))))
     (flycheck-fringe-info ((t (:bold t :weight bold :foreground "Green1"))))
     (flycheck-fringe-warning ((t (:bold t :weight bold :foreground "DarkOrange"))))
     (flycheck-info ((t (:bold t :weight bold :foreground "Green1" :underline t))))
     (flycheck-warning ((t (:bold t :weight bold :foreground "DarkOrange" :underline t))))
     (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-comment-delimiter-face ((t (:italic t :foreground "chocolate1" :slant italic))))
     (font-lock-comment-face ((t (:italic t :foreground "chocolate1" :slant italic))))
     (font-lock-constant-face ((t (:foreground "Aquamarine"))))
     (font-lock-doc-face ((t (:italic t :foreground "LightSalmon" :slant italic))))
     (font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))
     (font-lock-keyword-face ((t (:foreground "Cyan1"))))
     (font-lock-negation-char-face ((t (nil))))
     (font-lock-preprocessor-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
     (font-lock-regexp-grouping-construct ((t (:bold t :weight bold))))
     (font-lock-string-face ((t (:foreground "LightSalmon"))))
     (font-lock-type-face ((t (:foreground "PaleGreen"))))
     (font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
     (font-lock-warning-face ((t (:bold t :weight bold :foreground "Pink"))))
     (fringe ((t (:background "grey10"))))
     (git-diff-change ((t (:foreground "khaki"))))
     (git-diff-copy ((t (:foreground "green yellow"))))
     (git-diff-create ((t (:foreground "green"))))
     (git-diff-delete ((t (:foreground "red"))))
     (git-diff-rename ((t (:foreground "orange"))))
     (glyphless-char ((t (:underline t))))
     (haskell-constructor-face ((t (:foreground "PaleGreen"))))
     (haskell-debug-heading-face ((t (:foreground "Cyan1"))))
     (haskell-debug-keybinding-face ((t (:bold t :foreground "PaleGreen" :weight bold))))
     (haskell-debug-muted-face ((t (:foreground "#999"))))
     (haskell-debug-newline-face ((t (:bold t :background "#f0f0f0" :weight bold))))
     (haskell-debug-trace-number-face ((t (:bold t :background "#f5f5f5" :weight bold))))
     (haskell-debug-warning-face ((t (:bold t :foreground "DarkOrange" :weight bold))))
     (haskell-error-face ((t (:bold t :weight bold :foreground "Pink"))))
     (haskell-hole-face ((t (:bold t :weight bold :foreground "DarkOrange"))))
     (haskell-interactive-face-compile-error ((t (:bold t :foreground "Pink" :weight bold))))
     (haskell-interactive-face-compile-warning ((t (:bold t :foreground "DarkOrange" :weight bold))))
     (haskell-interactive-face-garbage ((t (:foreground "LightSalmon"))))
     (haskell-interactive-face-prompt ((t (:foreground "LightSkyBlue"))))
     (haskell-interactive-face-prompt2 ((t (:foreground "Cyan1"))))
     (haskell-interactive-face-result ((t (:foreground "LightSalmon"))))
     (haskell-keyword-face ((t (:foreground "Cyan1"))))
     (haskell-liquid-haskell-annotation-face ((t (:foreground "LightSteelBlue"))))
     (haskell-literate-comment-face ((t (:italic t :slant italic :foreground "LightSalmon"))))
     (haskell-operator-face ((t (:foreground "LightGoldenrod"))))
     (haskell-pragma-face ((t (:foreground "LightSteelBlue"))))
     (haskell-type-face ((t (:foreground "PaleGreen"))))
     (haskell-warning-face ((t (:bold t :weight bold :foreground "DarkOrange"))))
     (header-line ((t (:box (:line-width -1 :style released-button) :foreground "black" :background "grey75" :inverse-video nil :underline t))))
     (help-argument-name ((t (:italic t :slant italic))))
     (highlight ((t (:background "darkolivegreen"))))
     (hl-line ((t (:background "darkolivegreen"))))
     (info-header-node ((t (:italic t :bold t :weight bold :slant italic :foreground "white"))))
     (info-header-xref ((t (:foreground "cyan1" :underline t))))
     (info-index-match ((t (:background "RoyalBlue3"))))
     (info-menu-header ((t (:bold t :underline t :weight bold))))
     (info-menu-star ((t (:foreground "red1"))))
     (info-node ((t (:italic t :bold t :foreground "white" :slant italic :weight bold))))
     (info-title-1 ((t (:bold t :foreground "yellow" :weight bold))))
     (info-title-2 ((t (:bold t :foreground "lightblue" :weight bold))))
     (info-title-3 ((t (:bold t :weight bold))))
     (info-title-4 ((t (:bold t :weight bold))))
     (info-xref ((t (:underline t :foreground "cyan1"))))
     (info-xref-visited ((t (:foreground "violet" :underline t))))
     (isearch ((t (:background "palevioletred2" :foreground "brown4"))))
     (isearch-fail ((t (:background "red4"))))
     (italic ((t (:italic t :slant italic))))
     (js2-error ((t (:foreground "red"))))
     (js2-external-variable ((t (:foreground "orange"))))
     (js2-function-call ((t (:family "default" :foundry "default" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground sdh-unspecified-fg :background sdh-unspecified-bg :stipple nil :height 1))))
     (js2-function-param ((t (:foreground "SeaGreen"))))
     (js2-instance-member ((t (:foreground "DarkOrchid"))))
     (js2-jsdoc-html-tag-delimiter ((t (:foreground "green"))))
     (js2-jsdoc-html-tag-name ((t (:foreground "yellow"))))
     (js2-jsdoc-tag ((t (:foreground "SlateGray"))))
     (js2-jsdoc-type ((t (:foreground "SteelBlue"))))
     (js2-jsdoc-value ((t (:foreground "PeachPuff3"))))
     (js2-object-property ((t (:family "default" :foundry "default" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground sdh-unspecified-fg :background sdh-unspecified-bg :stipple nil :height 1))))
     (js2-private-function-call ((t (:foreground "goldenrod"))))
     (js2-private-member ((t (:foreground "PeachPuff3"))))
     (js2-warning ((t (:underline "orange"))))
     (lazy-highlight ((t (:background "paleturquoise4"))))
     (link ((t (:foreground "cyan1" :underline t))))
     (link-visited ((t (:underline t :foreground "violet"))))
     (markdown-blockquote-face ((t (:italic t :slant italic :foreground "LightSalmon"))))
     (markdown-bold-face ((t (:bold t :weight bold))))
     (markdown-code-face ((t (:family "Monospace"))))
     (markdown-comment-face ((t (:italic t :slant italic :foreground "chocolate1"))))
     (markdown-footnote-marker-face ((t (:weight normal :slant normal :foreground "grey70"))))
     (markdown-footnote-text-face ((t (:italic t :slant italic :foreground "chocolate1"))))
     (markdown-gfm-checkbox-face ((t (:foreground "LightSteelBlue"))))
     (markdown-header-delimiter-face ((t (:weight normal :slant normal :foreground "grey70"))))
     (markdown-header-face ((t (:bold t :foreground "LightSkyBlue" :weight bold))))
     (markdown-header-face-1 ((t (:bold t :weight bold :foreground "LightSkyBlue" :height 1.0))))
     (markdown-header-face-2 ((t (:bold t :weight bold :foreground "LightSkyBlue" :height 1.0))))
     (markdown-header-face-3 ((t (:bold t :weight bold :foreground "LightSkyBlue" :height 1.0))))
     (markdown-header-face-4 ((t (:bold t :weight bold :foreground "LightSkyBlue" :height 1.0))))
     (markdown-header-face-5 ((t (:bold t :weight bold :foreground "LightSkyBlue" :height 1.0))))
     (markdown-header-face-6 ((t (:bold t :weight bold :foreground "LightSkyBlue" :height 1.0))))
     (markdown-header-rule-face ((t (:weight normal :slant normal :foreground "grey70"))))
     (markdown-highlight-face ((t (:background "darkolivegreen"))))
     (markdown-hr-face ((t (:weight normal :slant normal :foreground "grey70"))))
     (markdown-inline-code-face ((t (:family "Monospace"))))
     (markdown-italic-face ((t (:italic t :slant italic))))
     (markdown-language-info-face ((t (:foreground "LightSalmon"))))
     (markdown-language-keyword-face ((t (:foreground "PaleGreen"))))
     (markdown-line-break-face ((t (:foreground "Aquamarine" :underline t))))
     (markdown-link-face ((t (:underline t :foreground "cyan1"))))
     (markdown-link-title-face ((t (:italic t :slant italic :foreground "chocolate1"))))
     (markdown-list-face ((t (:weight normal :slant normal :foreground "grey70"))))
     (markdown-markup-face ((t (:foreground "grey70" :slant normal :weight normal))))
     (markdown-math-face ((t (:foreground "LightSalmon"))))
     (markdown-metadata-key-face ((t (:foreground "LightGoldenrod"))))
     (markdown-metadata-value-face ((t (:foreground "LightSalmon"))))
     (markdown-missing-link-face ((t (:bold t :foreground "Pink" :weight bold))))
     (markdown-plain-url-face ((t (:foreground "cyan1" :underline t))))
     (markdown-pre-face ((t (:family "Monospace"))))
     (markdown-reference-face ((t (:weight normal :slant normal :foreground "grey70"))))
     (markdown-strike-through-face ((t (:strike-through t))))
     (markdown-url-face ((t (:foreground "LightSalmon"))))
     (match ((t (:background "RoyalBlue3"))))
     (menu ((t (nil))))
     (minibuffer-prompt ((t (:foreground "cyan"))))
     (mode-line ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))
     (mode-line-buffer-id ((t (:bold t :weight bold))))
     (mode-line-emphasis ((t (:bold t :weight bold))))
     (mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
     (mode-line-inactive ((t (:background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40" :style nil) :weight light))))
     (mouse ((t (nil))))
     (next-error ((t (:foreground "Black" :background "skyblue2"))))
     (nobreak-space ((t (:foreground "cyan" :underline t))))
     (outline-1 ((t (:foreground "LightSkyBlue"))))
     (outline-2 ((t (:foreground "LightGoldenrod"))))
     (outline-3 ((t (:foreground "Cyan1"))))
     (outline-4 ((t (:italic t :slant italic :foreground "chocolate1"))))
     (outline-5 ((t (:foreground "PaleGreen"))))
     (outline-6 ((t (:foreground "Aquamarine"))))
     (outline-7 ((t (:foreground "LightSteelBlue"))))
     (outline-8 ((t (:foreground "LightSalmon"))))
     (p4-action-face ((t (:bold t :weight bold))))
     (p4-branch-face ((t (:bold t :weight bold))))
     (p4-change-face ((t (:bold t :weight bold))))
     (p4-client-face ((t (:bold t :weight bold))))
     (p4-depot-add-face ((t (:foreground "cyan"))))
     (p4-depot-branch-face ((t (:foreground "sky blue"))))
     (p4-depot-delete-face ((t (:foreground "pink"))))
     (p4-depot-edit-face ((t (:foreground "light green"))))
     (p4-description-face ((t (nil))))
     (p4-filespec-face ((t (:bold t :weight bold))))
     (p4-form-comment-face ((t (:italic t :slant italic :foreground "chocolate1"))))
     (p4-form-keyword-face ((t (:foreground "Cyan1"))))
     (p4-heading-face ((t (nil))))
     (p4-job-face ((t (:bold t :weight bold))))
     (p4-label-face ((t (:bold t :weight bold))))
     (p4-link-face ((t (:bold t :weight bold))))
     (p4-revision-face ((t (:bold t :weight bold))))
     (p4-user-face ((t (:bold t :weight bold))))
     (phi-replace-preview-face ((t (:background "darkolivegreen"))))
     (phi-search-failpart-face ((t (:background "red4"))))
     (phi-search-match-face ((t (:background "#194854"))))
     (phi-search-selection-face ((t (:background "#594854"))))
     (pp^L-highlight ((t (nil))))
     (print-tag-face ((t (:foreground "brightyellow"))))
     (query-replace ((t (:foreground "brown4" :background "palevioletred2"))))
     (rectangle-preview-face ((t (:foreground "Black" :background "skyblue2"))))
     (region ((t (:background "skyblue2" :foreground "Black"))))
     (scroll-bar ((t (nil))))
     (secondary-selection ((t (:background "SkyBlue4"))))
     (sgml-namespace ((t (:foreground "LightSteelBlue"))))
     (sh-escaped-newline ((t (:foreground "LightSalmon"))))
     (sh-heredoc ((t (:bold t :foreground "yellow1" :weight bold))))
     (sh-quoted-exec ((t (:foreground "salmon"))))
     (shadow ((t (:foreground "grey70"))))
     (show-paren-match ((t (:background "steelblue3"))))
     (show-paren-mismatch ((t (:background "purple" :foreground "white"))))
     (smerge-base ((t (:background "#888833"))))
     (smerge-markers ((t (:background "grey30"))))
     (smerge-mine ((t (:background "#553333"))))
     (smerge-other ((t (:background "#335533"))))
     (smerge-refined-added ((t (:background "#22aa22"))))
     (smerge-refined-changed ((t (nil))))
     (smerge-refined-removed ((t (:background "#aa2222"))))
     (speedbar-button-face ((t (:foreground "green3"))))
     (speedbar-directory-face ((t (:foreground "light blue"))))
     (speedbar-file-face ((t (:foreground "cyan"))))
     (speedbar-highlight-face ((t (:background "sea green"))))
     (speedbar-selected-face ((t (:foreground "red" :underline t))))
     (speedbar-separator-face ((t (:background "blue" :foreground "white" :overline "gray"))))
     (speedbar-tag-face ((t (:foreground "yellow"))))
     (success ((t (:bold t :foreground "Green1" :weight bold))))
     (term ((t (:family "default" :foundry "default" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground sdh-unspecified-fg :background sdh-unspecified-bg :stipple nil :height 1))))
     (term-bold ((t (:bold t :weight bold))))
     (term-color-black ((t (:background "black" :foreground "black"))))
     (term-color-blue ((t (:background "blue2" :foreground "blue2"))))
     (term-color-cyan ((t (:background "cyan3" :foreground "cyan3"))))
     (term-color-green ((t (:background "green3" :foreground "green3"))))
     (term-color-magenta ((t (:background "magenta3" :foreground "magenta3"))))
     (term-color-red ((t (:background "red3" :foreground "red3"))))
     (term-color-white ((t (:background "white" :foreground "white"))))
     (term-color-yellow ((t (:background "yellow3" :foreground "yellow3"))))
     (term-underline ((t (:underline t))))
     (tool-bar ((t (:foreground "black" :box (:line-width 1 :style released-button)))))
     (tooltip ((t (:family "Sans Serif" :background "lightyellow" :foreground "black"))))
     (trailing-whitespace ((t (:background "color-160" :foreground "Black"))))
     (tty-menu-disabled-face ((t (:background "blue" :foreground "lightgray"))))
     (tty-menu-enabled-face ((t (:bold t :background "blue" :foreground "yellow" :weight bold))))
     (tty-menu-selected-face ((t (:background "red"))))
     (underline ((t (:underline t))))
     (variable-pitch ((t (:family "Sans Serif"))))
     ;; (vc-conflict-state ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))
     ;; (vc-edited-state ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))
     ;; (vc-locally-added-state ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))
     ;; (vc-locked-state ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))
     ;; (vc-missing-state ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))
     ;; (vc-needs-update-state ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))
     ;; (vc-removed-state ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))
     ;; (vc-state-base-face ((t (:box (:line-width -1 :style released-button) :foreground "black" :background "grey75"))))
     ;; (vc-up-to-date-state ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))
     (vertical-border ((t (:weight light :box (:line-width -1 :color "grey40" :style nil) :foreground "grey80" :background "grey30"))))
     (vr/group-0 ((t (:background "blue3"))))
     (vr/group-1 ((t (:background "chartreuse4"))))
     (vr/group-2 ((t (:background "sienna4"))))
     (vr/match-0 ((t (:background "steelblue4"))))
     (vr/match-1 ((t (:background "dodgerblue4"))))
     (vr/match-separator-face ((t (:bold t :foreground "red" :weight bold))))
     (w3m-haddock-heading-face ((t (:background "darkolivegreen"))))
     (warning ((t (:bold t :foreground "DarkOrange" :weight bold))))
     (widget-button ((t (:bold t :weight bold))))
     (widget-button-pressed ((t (:foreground "red1"))))
     (widget-documentation ((t (:foreground "lime green"))))
     (widget-field ((t (:background "yellow3" :foreground "black"))))
     (widget-inactive ((t (:foreground "grey70"))))
     (widget-single-line-field ((t (:background "green3" :foreground "black"))))
     (window-divider ((t (:foreground "gray60"))))
     (window-divider-first-pixel ((t (:foreground "gray80"))))
     (window-divider-last-pixel ((t (:foreground "gray40")))))))
(add-to-list 'color-themes '(sdh-color-theme  "sdh" "Stephen Hicks"))

(provide 'sdh-color-theme)