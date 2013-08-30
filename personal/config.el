;; Deft
(setq deft-text-mode 'markdown-mode)

;; Color theme
(load-theme 'base16-eighties)

;; Hide menu bar
(menu-bar-mode -1)

;; Hide Scroll Bar
(scroll-bar-mode -1)

;; Tab or complete
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key [tab] 'tab-indent-or-complete)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)

;; Whitespace
(global-whitespace-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Coffee-Tab Width
(setq coffee-tab-width 2)
