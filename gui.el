;; Theme
(load-theme 'material t)

;; Set the color of the fringe
(let ((background-color (face-attribute 'default :background)))
  (set-face-background 'fringe background-color))

(add-to-list 'default-frame-alist '(font . "Monaco-16"))
