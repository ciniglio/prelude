(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode)

(add-to-list 'load-path "~/.emacs.d/")

;; Always prefer to load newer files, instead of giving precedence to
;; the .elc files.
(setq load-prefer-newer t)
(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(load "naked.el")
(load "basics.el")
(load "programming_modes.el")
(load "keyboard_shortcuts.el")
(load "gui.el")

(require 'use-package)

(use-package org
  :config (progn
            (define-key org-mode-map [C-tab] 'other-window)
            (define-key org-mode-map [C-S-tab]
              (lambda ()
                (interactive)
                (other-window -1)))))

(use-package whitespace
  :config
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; only show bad whitespace
  (setq whitespace-style '(face trailing space-before-tab tabs empty space-after-tab))
  :init
  (global-whitespace-mode 1)
  (diminish 'global-whitespace-mode))

(use-package company
  :config (progn
            (setq company-minimum-prefix-length 2)
            (setq company-idle-delay .1)
            (setq company-tooltip-limit 10)
            (setq company-tooltip-flip-when-above t)
            (setq company-dabbrev-downcase nil)
            (set-face-attribute 'company-echo-common nil
                                :foreground "selectedMenuItemColor")
            (set-face-attribute 'company-tooltip nil
                                :background "knobColor"
                                :foreground "selectedMenuItemTextColor")
            (set-face-attribute 'company-preview-common nil
                                :inherit 'company-preview
                                :foreground "selectedMenuItemColor")
            (set-face-attribute 'company-preview-search nil
                                :inherit 'company-preview
                                :background "alternateSelectedControlColor")
            (set-face-attribute 'company-scrollbar-bg nil
                                :inherit 'company-tooltip
                                :background "knobColor")
            (set-face-attribute 'company-scrollbar-fg nil
                                :background "scrollBarColor")
            (set-face-attribute 'company-tooltip-annotation nil
                                :inherit 'company-tooltip
                                :foreground "selectedControlTextColor")
            (set-face-attribute 'company-tooltip-common nil
                                :inherit 'company-tooltip
                                :foreground "alternateSelectedControlColor")
            (set-face-attribute 'company-tooltip-common-selection nil
                                :inherit 'company-tooltip-selection
                                :foreground "selectedTextColor")
            (set-face-attribute 'company-tooltip-selection nil
                                :inherit 'company-tooltip
                                :background "selectedMenuItemColor"))
  :init (progn
          (global-company-mode)
          (diminish 'company-mode " /c")))

(use-package ido
  :config (setq ido-use-faces nil)
  :init (progn
          (ido-mode 1)
          (ido-everywhere 1)
          (use-package ido-vertical-mode
            :init (ido-vertical-mode 1))
          (use-package flx-ido
            :init (flx-ido-mode 1))))

(use-package helm
  :config (progn
            (setq helm-adaptive-mode t)
	    (setq helm-split-window-in-side-p t)
            (setq helm-mini-default-sources
                  (quote
                   (helm-source-buffers-list
                    helm-source-recentf
                    helm-source-projectile-projects
                    helm-source-buffer-not-found)))
            (set-face-attribute 'helm-selection nil
                                :background "medium sea green"
                                :distant-foreground "black")
            (set-face-attribute 'helm-source-header nil
                                :background "gray12"
                                :foreground "headerColor"
                                :weight 'bold
                                :height 1.4
                                :family "Sans Serif")
            (set-face-attribute 'helm-visible-mark nil
                                :background "disabledControlTextColor"
                                :foreground "selectedTextColor"))
  :init (use-package projectile
          :requires (helm)
          :config (progn
                    (setq projectile-mode-line
                          (quote
                           (" P" (:eval (format "|%s|"
                                                (projectile-project-name))))))
                    (setq projectile-completion-system 'helm)
                    (define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
                    (define-key projectile-mode-map [?\s-p] 'projectile-switch-project)
                    (define-key projectile-mode-map [?\s-f] 'projectile-find-file)
                    (define-key projectile-mode-map [?\s-g] 'projectile-grep)
                    (define-key projectile-mode-map [?\s-b] 'projectile-switch-to-buffer))
          :init
          (projectile-global-mode)
          (helm-projectile-on))
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("C-h a" . helm-apropos)
         ("C-x b" . helm-buffers-list)))



(use-package hydra
  :requires (helm projectile helm-projectile counsel key-chord)
  :config (progn
            (defhydra hydra-projectile
              (:color blue)
              "projects and common tasks"
              ("h" helm-mini "mini")
              ("p" helm-projectile "projects")
              ("g" counsel-git-grep "grep")
              ("f" helm-projectile-find-file-dwim "files"))
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
            (define-key projectile-mode-map [?\s-h] 'hydra-projectile/body)
            (key-chord-define-global "hs" (lambda ()
                                            (interactive)
                                            (hs-minor-mode t)
                                            (hydra-hide-show/body)))))

(use-package sane-term
  :init (progn
         (message "Using sane term")
         (setq sane-term-shell-command "/bin/zsh"))
  :config (progn
            (add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))
            (defun my/sane-term (arg)
             (interactive "P")
             (if arg
                 (sane-term-create)
               (sane-term))))
  :bind (("M-m" . my/sane-term)))

(setq-default dired-listing-switches "-alh")

;; (use-package ansi-term
;;   :init (message "Using ansi term"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("75c0b1d2528f1bce72f53344939da57e290aa34bea79f3a1ee19d6808cb55149" "f15a7ce08b9e13553c1f230678e9ceb5b372f8da26c9fb815eb20df3492253b7" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" default)))
 '(deft-use-filename-as-title t)
 '(erc-prompt " >")
 '(global-company-mode t)
 '(ido-use-faces t)
 '(ns-use-srgb-colorspace nil)
 '(powerline-default-separator (quote contour))
 '(powerline-utf-8-separator-left 57520)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "white"))))
 '(hl-line ((t (:background "gray15"))))
 '(hydra-face-blue ((t (:foreground "selectedMenuItemColor" :weight bold))))
 '(quote (fringe ((t (:background "white"))))))

(setenv "BOOT_JVM_OPTIONS" "");"-XX:MaxPermSize=512M")
(put 'downcase-region 'disabled nil)
