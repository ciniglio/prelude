(powerline-default-theme)
(load-theme 'base16-eighties t)

(smart-cursor-color-mode +1)
;; Set the color of the fringe
(custom-set-faces
 '(fringe ((t (:background "#393939")))))

;; God mode toggle box / bar cursor
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
			'box
		      'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
