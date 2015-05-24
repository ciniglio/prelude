(defhydra hydra-projectile
   (:color blue)
   "projectile"
   ("h" helm-mini "mini")
   ("p" helm-projectile "projects")
   ("c" helm-circe "irc")
   ("f" helm-projectile-find-file-dwim "files"))

(define-key projectile-mode-map [?\s-h] 'hydra-projectile/body)
(global-ace-isearch-mode +1)


(defhydra hydra-hide-show
  (:color pink)
  "h-s"
  ("TAB" hs-toggle-hiding)
  ("h" hs-hide-block "hide")
  ("s" hs-show-block "show")
  ("H" hs-hide-all "hide all")
  ("S" hs-show-all "hide all")
  ("n" next-line)
  ("p" previous-line)
  ("q" nil :exit t))

(key-chord-define-global "hs" (lambda ()
                                (interactive)
                                (hs-minor-mode t)
                                (hydra-hide-show/body)))
