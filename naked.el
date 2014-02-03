;; I'm never typing yes or no out
(defalias 'yes-or-no-p 'y-or-n-p)

;; Prevent the cursor from blinking
(blink-cursor-mode 0)
;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
;; Don't let Emacs hurt your ears
(setq visible-bell t)

;; You need to set `inhibit-startup-echo-area-message' from the
;; customization interface:
;; M-x customize-variable RET inhibit-startup-echo-area-message RET
;; then enter your username
(setq inhibit-startup-echo-area-message "alejandro")

;; Set the color of the fringe
(custom-set-faces
 '(fringe ((t (:background "white")))))

;; Dumb autosave files
;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;; Bigger
(when (window-system)
  (set-frame-height (selected-frame) 65)
  (set-frame-width (selected-frame) 230))

(setq default-frame-alist
      '((width . 230) (height . 65)))

;; Mode line
(defun my-remove-mm-indicator (mm)
  "Remove minor indicator from the mode line."
  (setcar (cdr (assq mm minor-mode-alist)) nil))

(add-hook 'undo-tree-mode-hook
	  (lambda () (my-remove-mm-indicator 'undo-tree-mode)))

(add-hook 'volatile-highlights-mode-hook
	  (lambda () (my-remove-mm-indicator 'volatile-highlights-mode)))

;; This is bound to f11 in Emacs 24.4
;(toggle-frame-fullscreen)
;; Who use the bar to scroll?
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
