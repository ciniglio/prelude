(powerline-default-theme)
(load-theme 'base16-tomorrow t)

;; Set the color of the fringe
(let ((background-color (face-attribute 'default :background)))
 (custom-set-faces
  `(fringe ((t (:background ,background-color))))))
