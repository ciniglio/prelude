(defhydra hydra-projectile
   (:color blue)
   "projectile"
   ("h" helm-mini "mini")
   ("p" helm-projectile "projects")
   ("f" helm-projectile-find-file-dwim "files"))

(define-key projectile-mode-map [?\s-h] 'hydra-projectile/body)
(global-ace-isearch-mode +1)
