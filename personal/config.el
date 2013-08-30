;;; personal/config.el -- alejandro's personal configuration for emacs
;;
;; Alejandro Ciniglio 2013
;;


;;;   Appearance
;; Color theme
(load-theme 'base16-chalk)

;; Hide menu bar
(menu-bar-mode -1)

;; Hide Scroll Bar
(scroll-bar-mode -1)


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


;;; Key maps
;; Tab or complete helper
(global-set-key [tab] 'tab-indent-or-complete)
