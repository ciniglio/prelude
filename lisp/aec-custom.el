;;; Customization
(defconst aec-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit
  :config
  (setq custom-file aec-custom-file
        custom-buffer-done-kill nil            ; Kill when existing
        custom-buffer-verbose-help nil         ; Remove redundant help text
        ;; Show me the real variable name
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil)
  :init (load aec-custom-file 'no-error 'no-message))

;; Scrolling
(setq scroll-conservatively 10000
      scroll-preserve-screen-position t)

(blink-cursor-mode 0)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t)
(fset 'yes-or-no-p #'y-or-n-p)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default indent-tabs-mode nil)

(provide 'aec-custom)
