;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile
(require 'projectile)
(setq projectile-mode-line-lighter " P")
(projectile-update-mode-line)
(projectile-global-mode)
(setq projectile-remember-window-configs t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace mode
;; automatically clean up bad whitespace
(setq whitespace-action '(auto-cleanup))
;; only show bad whitespace
(setq whitespace-style '(face trailing space-before-tab indentation empty space-after-tab))
;; enable
(global-whitespace-mode 1)
(diminish 'global-whitespace-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coffee Mode
;; This gives you a tab of 2 spaces
(custom-set-variables '(coffee-tab-width 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yas
(yas-global-mode 1)
(diminish 'yas-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undo
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delims
(global-rainbow-delimiters-mode)

(require 'smartparens-config)
(require 'smartparens-ruby)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
(show-smartparens-global-mode +1)
(smartparens-global-mode +1)

(dolist (x '(scheme emacs-lisp lisp clojure cider-repl))
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) '(lambda () (smartparens-strict-mode 1))))

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;;; html-mode
(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

;;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Css
;; Automatically activate rainbow-mode for CSS files
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'sass-mode-hook 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple cursors
(require 'multiple-cursors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files and mode
(dolist (exp '("Rakefile\\'" "\\.rake\\'"))
  (add-to-list 'auto-mode-alist
	       (cons exp 'ruby-mode)))

(dolist (exp '("\\zshrc\\'" "\\bashrc\\'"))
  (add-to-list 'auto-mode-alist
	       (cons exp 'ruby-mode)))

(dolist (exp '("Cask\\'"))
  (add-to-list 'auto-mode-alist
	       (cons exp 'emacs-lisp-mode)))

(dolist (exp '("\\.hamlc\\'"))
  (add-to-list 'auto-mode-alist
	       (cons exp 'haml-mode)))

(require 'web-mode)
(dolist (exp '("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.jsp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.html?\\'" "\\.mustache\\'" "\\.djhtml\\'"))
  (add-to-list 'auto-mode-alist
	       (cons exp 'web-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flyspell
(defun turn-on-flyspell ()
  (progn
    (flyspell-mode 1)
    (diminish 'flyspell-mode)))
(add-hook 'find-file-hooks 'turn-on-flyspell)

(defun turn-on-flyspell-prog ()
  (progn
    (flyspell-prog-mode)
    (diminish 'flyspell-mode)))

(mapcar (lambda (mode-hook) (add-hook mode-hook 'turn-on-flyspell-prog))
	'(c-mode-common-hook tcl-mode-hook emacs-lisp-mode-hook
	  ruby-mode-hook java-mode-hook clojure-mode-hook
	  coffee-mode-hook haml-mode-hook web-mode-hook sass-mode-hook))
