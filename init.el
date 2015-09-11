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
          (diminish 'company-mode)))

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
            (require 'helm-config)
            (setq helm-adaptive-mode t
                  helm-split-window-in-side-p t
                  completing-read-function 'helm--completing-read-default
                  helm-ff-auto-update-initial-value t
                  helm-ff--auto-update-state t)
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
                                :foreground "selectedTextColor")
            (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
            (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
            (define-key helm-map (kbd "C-z")  'helm-select-action)  ; list actions using C-z
            (diminish 'helm-mode))
  :init (progn
          (with-eval-after-load 'helm-files
            (define-key helm-read-file-map (kbd "<backspace>") 'helm-find-files-up-one-level)
            (define-key helm-find-files-map (kbd "<backspace>") 'helm-find-files-up-one-level))
         (use-package projectile
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
           (helm-projectile-on)))
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("C-h a" . helm-apropos)
         ("M-i" . helm-imenu)
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
            (defhydra hydra-search (:color blue)
              "Search"
              ("s" helm-swoop "Swoop")
              ("d" helm-multi-swoop "Multi-swoop")
              ("g" helm-do-grep "Grep")
              ("h" helm-org-headlines "Org Headlines")
              ("m" helm-multi-occur "Multi-occur")
              ("o" helm-occur "Occur"))

            (define-key projectile-mode-map [?\s-h] 'hydra-projectile/body)
            (key-chord-define-global "hs" (lambda ()
                                            (interactive)
                                            (hs-minor-mode t)
                                            (hydra-hide-show/body)))
            (global-set-key (kbd "<f2>") 'hydra-search/body)))

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

(use-package which-key
  :config (progn
            (which-key-mode)
            (diminish 'which-key-mode)))

(use-package diff-hl
  :config (diff-hl-mode))

(use-package smart-mode-line
  :config
  (progn
    (smart-mode-line-enable)
    (sml/apply-theme 'smart-mode-line-respectful)))

(use-package org
  :config (setq org-directory "~/Documents/org/"
                org-agenda-files (list org-directory)
                org-default-notes-file "~/Documents/org/inbox.org"
                org-capture-templates
                '(("n" "Note" entry (file+headline "~/Documents/org/inbox.org" "Inbox")
                   "* TODO %<%Y-%m-%d %H:%M:%S>\n\n%?" :empty-lines 1)
                  ("w" "Work" entry (file+datetree "~/Documents/org/work.org")
                   "* %<%H:%M>\n\n%?" :empty-lines 1)
                  ("j" "Journal" entry (file+datetree "~/Documents/org/journal.org")
                   "* %<%H:%M>\n\n%?" :empty-lines 1))))

(use-package yasnippet
  :config (progn
            (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
            (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))
            (setq yas-wrap-around-region t)
            (setq yas-verbosity 1)
            (yas-global-mode 1)
            (defun yas/goto-end-of-active-field ()
              (interactive)
              (let* ((snippet (car (yas--snippets-at-point)))
                     (position (yas--field-end (yas--snippet-active-field snippet))))
                (if (= (point) position)
                    (move-end-of-line 1)
                  (goto-char position))))

            (defun yas/goto-start-of-active-field ()
              (interactive)
              (let* ((snippet (car (yas--snippets-at-point)))
                     (position (yas--field-start (yas--snippet-active-field snippet))))
                (if (= (point) position)
                    (move-beginning-of-line 1)
                  (goto-char position))))
            (define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
            (define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)))

(use-package clojure-mode
  :mode (("\\.edn$" . clojure-mode)
         ("\\.cljc$" . clojure-mode)
         ("\\.boot$" . clojure-mode))
  :init
  (progn
    (add-hook 'clojure-mode-hook '(lambda () (smartparens-strict-mode 1)))
    (add-hook 'clojure-mode-hook 'turn-on-flyspell-prog)
    (add-hook 'clojure-mode-hook 'prettify-symbols-mode)
    (use-package cider
      :init
      (progn
        (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
        (add-hook 'cider-repl-mode-hook 'subword-mode)
        (add-hook 'cider-repl-mode-hook '(lambda () (smartparens-strict-mode 1))))
      :config
      (progn
        (setq cider-show-error-buffer 't)
        (setq nrepl-hide-special-buffers t)
        (setq cider-popup-stacktraces-in-repl t)
        (setq cider-repl-history-file "~/.emacs.d/nrepl-history")
        (setq cider-repl-pop-to-buffer-on-connect nil)
        (setq cider-repl-use-clojure-font-lock t)
        (setq cider-auto-select-error-buffer nil)
        (setq cider-prompt-save-file-on-load nil)
        (define-key cider-mode-map (kbd "C-c C-w") 'nil)
        (define-key cider-repl-mode-map (kbd "C-c C-w") 'nil))))
  :config
  (progn
    (define-clojure-indent
      (defroutes 'defun)
      (GET 2)
      (POST 2)
      (PUT 2)
      (DELETE 2)
      (HEAD 2)
      (ANY 2)
      (context 2))

    (define-clojure-indent
      (form-to 1))

    (define-clojure-indent
      (match 1)
      (are 2))

    (define-clojure-indent
      (select 1)
      (insert 1)
      (update 1)
      (delete 1))

    (define-clojure-indent
      (run* 1)
      (fresh 1))

    (define-clojure-indent
      (extend-freeze 2)
      (extend-thaw 1))

    (define-clojure-indent
      (go-loop 1))

    (define-clojure-indent
      (this-as 1)
      (specify 1)
      (specify! 1))

    (setq clojure--prettify-symbols-alist
          '(("fn"  . ?λ)
            ("comp" . ?∘)
            ("not=" . ?≠)
            ("<=" . ?≤)
            (">=" . ?≥)
            ("identical?" . ?≡)))

    (defun toggle-nrepl-buffer ()
      "Toggle the nREPL REPL on and off"
      (interactive)
      (if (string-match "cider-repl" (buffer-name (current-buffer)))
          (delete-window)
        (cider-switch-to-relevant-repl-buffer)))

    (defun cider-project-reset ()
      (interactive)
      (cider-interactive-eval "(reloaded.repl/reset)"))))

(use-package clj-refactor
  :init
  (add-hook 'clojure-mode-hook (lambda () (progn
                                            (clj-refactor-mode 1)
                                            (yas-minor-mode 1)
                                            (cljr-add-keybindings-with-prefix "C-c C-m")))))
(use-package eyebrowse
  :config (progn
            (diminish 'eyebrowse-mode)
            (eyebrowse-mode)))

(setq-default dired-listing-switches "-alh")
(set-default 'indent-tabs-mode nil)
;; (use-package ansi-term
;;   :init (message "Using ansi term"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(deft-use-filename-as-title t)
 '(global-company-mode t)
 '(helm-projectile-sources-list
   (quote
    (helm-source-projectile-buffers-list helm-source-projectile-recentf-list helm-source-projectile-files-list helm-source-projectile-projects)))
 '(ido-use-faces t)
 '(ns-use-srgb-colorspace nil)
 '(powerline-default-separator (quote contour))
 '(powerline-utf-8-separator-left 57520)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(web-mode-markup-indent-offset 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "gray15"))))
 '(hydra-face-blue ((t (:foreground "selectedMenuItemColor" :weight bold)))))

(setenv "BOOT_JVM_OPTIONS" "");"-XX:MaxPermSize=512M")
(put 'downcase-region 'disabled nil)
