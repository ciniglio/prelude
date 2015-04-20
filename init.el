(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode)

(add-to-list 'load-path "~/.emacs.d/")
(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(load "naked.el")
(load "basics.el")
(load "autocompletes.el")
(load "programming_modes.el")
(load "keyboard_shortcuts.el")
(load "gui.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" default)))
 '(helm-adaptive-mode t nil (helm-adaptive))
 '(magit-default-tracking-name-function (quote magit-default-tracking-name-branch-only))
 '(powerline-utf-8-separator-left 57520)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "gray15")))))
