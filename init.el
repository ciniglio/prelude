(require 'cask "~/.cask/cask.el")
(cask-initialize)

(add-to-list 'load-path "~/.emacs.d/")
(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(load "naked.el")
(load "basics.el")
(load "autocompletes.el")
(load "programming_modes.el")
(load "keyboard_shortcuts.el")
(load "gui.el")
