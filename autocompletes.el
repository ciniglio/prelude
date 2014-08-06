;; Autocompletes in most menus
(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)
(smex-initialize)

(setq company-minimum-prefix-length 2)
(setq company-idle-delay .1)
(global-company-mode)
