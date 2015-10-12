;; Theme
(defun my/setup-color-theme ()
  (interactive)
  (load-theme 'material t)
  ;; Set the color of the fringe
  (let ((background-color (face-attribute 'default :background)))
    (set-face-background 'fringe background-color)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
        (lambda (frame)
            (select-frame frame)
            (my/setup-color-theme)))
  (my/setup-color-theme))

(add-to-list 'default-frame-alist '(font . "Monaco-16"))
