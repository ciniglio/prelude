;; Autocompletes in most menus
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)
(smex-initialize)

;; Auto-complete
(require 'auto-complete-config)
(ac-config-default)

(add-hook 'cider-mode-hook 'ac-cider-compliment-setup)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))
