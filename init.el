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
(load "my_hydras.el")
(load "circe_config.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("f15a7ce08b9e13553c1f230678e9ceb5b372f8da26c9fb815eb20df3492253b7" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" default)))
 '(deft-use-filename-as-title t)
 '(helm-adaptive-mode t nil (helm-adaptive))
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-source-recentf helm-source-projectile-projects helm-source-buffer-not-found)))
 '(magit-default-tracking-name-function (quote magit-default-tracking-name-branch-only))
 '(ns-use-srgb-colorspace nil)
 '(powerline-utf-8-separator-left 57520)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-selection ((t (:background "medium sea green" :distant-foreground "black"))))
 '(helm-source-header ((t (:background "gray12" :foreground "headerColor" :weight bold :height 1.4 :family "Sans Serif"))))
 '(helm-visible-mark ((t (:background "disabledControlTextColor" :foreground "selectedTextColor"))))
 '(hl-line ((t (:background "gray15"))))
 '(hydra-face-blue ((t (:foreground "selectedMenuItemColor" :weight bold)))))
