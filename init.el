(require 'cask "~/.cask/cask.el")
(cask-initialize)

(add-to-list 'load-path "~/.emacs.d/")

(load "naked.el")

(load "autocompletes.el")
(load "programming_modes.el")
(load "keyboard_shortcuts.el")
(load "gui.el")

(delete-selection-mode 1)
