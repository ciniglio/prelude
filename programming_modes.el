;; Projectile
(projectile-global-mode)
(setq projectile-remember-window-configs t)

;; Whitespace mode
;; automatically clean up bad whitespace
(setq whitespace-action '(auto-cleanup))
;; only show bad whitespace
(setq whitespace-style '(face trailing space-before-tab indentation empty space-after-tab))
;; enable
(global-whitespace-mode 1)

;; Coffee Mode
;; This gives you a tab of 2 spaces
(custom-set-variables '(coffee-tab-width 2))

;; yas
(yas-global-mode 1)

;; Undo
(global-undo-tree-mode)
