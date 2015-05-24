;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile
(require 'projectile)

(setq projectile-mode-line
   (quote
    (" P" (:eval (format "|%s|"
                         (projectile-project-name))))))

(projectile-global-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace mode
;; automatically clean up bad whitespace
(setq whitespace-action '(auto-cleanup))
;; only show bad whitespace
(setq whitespace-style '(face trailing space-before-tab tabs empty space-after-tab))
;; enable
(global-whitespace-mode 1)
(diminish 'global-whitespace-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coffee Mode
(eval-after-load 'coffee-mode
  '(progn
     ;; CoffeeScript uses two spaces.
     (setq coffee-tab-width 2)

     (defun t-coffee-mode-default ()
       (subword-mode +1))

  (add-hook 'coffee-mode-hook 't-coffee-mode-defaults)))

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
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'smartparens-config)
(require 'smartparens-ruby)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)

(sp-local-pair 'enh-ruby-mode "{" nil :post-handlers '(:add " | "))
(sp-local-pair 'ruby-mode "{" nil :post-handlers '(:add " | "))

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
;; Cider
;;
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-show-error-buffer 't)

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
(dolist (exp '("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.jsp\\'" "\\.as[cp]x\\'"
               "\\.erb\\'" "\\.html?\\'" "\\.mustache\\'" "\\.djhtml\\'"
               "\\.jsx\\'"))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
(require 'magit)
(magit-auto-revert-mode)
(setq magit-last-seen-setup-instructions "1.4.0")
(diminish 'magit-auto-revert-mode "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc diminishes
(defvar mode-line-cleaner-alist
  `(;; Major modes
    (lisp-interaction-mode . "λ")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (emacs-lisp-mode . "EL")
    (nxhtml-mode . "nx"))
  "Alist for `clean-mode-line'.
When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;; alias the new `flymake-report-status-slim' to
;;; `flymake-report-status'
(defalias 'flymake-report-status 'flymake-report-status-slim)
(defun flymake-report-status-slim (e-w &optional status)
  "Show \"slim\" flymake status in mode line."
  (when e-w
    (setq flymake-mode-line-e-w e-w))
  (when status
    (setq flymake-mode-line-status status))
  (let* ((mode-line " Φ"))
    (when (> (length flymake-mode-line-e-w) 0)
      (setq mode-line (concat mode-line ":" flymake-mode-line-e-w)))
    (setq mode-line (concat mode-line flymake-mode-line-status))
    (setq flymake-mode-line mode-line)
    (force-mode-line-update)))
