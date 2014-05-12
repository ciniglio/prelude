(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

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
 '(magit-default-tracking-name-function (quote magit-default-tracking-name-branch-only))
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify)))
