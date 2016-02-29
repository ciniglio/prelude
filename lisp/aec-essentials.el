;;; aec-essentials.el --- pre-runs for init

;;; Commentary:
;; Contains things that absolutely need to be loaded before anything
;; else in the init file. Assumes that packages are initialized.

;;; Code:
(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

(use-package hydra
  :ensure t)

(provide 'aec-essentials)
;;; aec-essentials.el ends here
