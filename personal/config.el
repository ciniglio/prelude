;;; personal/config.el -- alejandro's personal configuration for emacs
;;
;; Alejandro Ciniglio 2013
;;

;;;   Appearance
;; Color theme
(load-theme 'base16-chalk t)

;; Hide menu bar
(menu-bar-mode -1)

;; Hide Scroll Bar
(scroll-bar-mode -1)

;; Ansi-term glitch
(setq system-uses-terminfo nil)

;;; Mode Config
;; Whitespace
(global-whitespace-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Rainbow Delimiters
(global-rainbow-delimiters-mode 1)

;; Fixes some dumb coffee-mode behavior
(setq coffee-tab-width 2)

;; Deft
(setq deft-text-mode 'markdown-mode)

;; Company (completion)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)

;; Magit
(setq magit-completing-read-function
      'magit-ido-completing-read)

;;; Key maps
;; Tab or complete helper
(require 'smart-tab)
(global-smart-tab-mode 1)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(key-chord-define-global "GG" 'magit-status)

;;; Hooks
;; Coffee Mode
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;; Haml Mode
(add-to-list 'auto-mode-alist '("\\.hamlc$" . haml-mode))

;; Go Mode
;; (add-hook 'before-save-hook #'gofmt-before-save)

;; Ruby Mode
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
