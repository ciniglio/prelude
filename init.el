(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package aec-essentials :load-path "lisp/")
(use-package aec-custom :load-path "lisp/")
(use-package aec-backup :load-path "lisp/")

(use-package ciniglio-site-lisp
  :if (file-exists-p "~/.ciniglio-site-specific/site-lisp")
  :load-path "~/.ciniglio-site-specific/site-lisp/")

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config (setq company-tooltip-align-annotations t
		company-tooltip-flip-when-above t
		company-idle-delay .1
		company-require-match nil)
  :diminish company-mode)

(use-package company-quickhelp
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
	  (company-quickhelp-mode)))

(use-package vc-hooks
  :defer t
  :config (setq vc-follow-symlinks t))

(use-package helm
  :ensure t
  :bind (("C-c c b" . helm-resume))
  :init (helm-mode 1)
  :config (progn
	    (setq helm-split-window-in-side-p t)
	    (set-face-attribute 'helm-selection-line nil
				:background "#ba55d3")
	    (set-face-attribute 'helm-selection nil
				:background "#ba55d3")
            (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
            (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
            (define-key helm-map (kbd "C-z")  'helm-select-action))
  :diminish helm-mode)

(use-package helm-misc
  :ensure helm
  :bind (([remap switch-to-buffer] . helm-mini)))

(use-package helm-adaptive-mode
  :ensure helm
  :config (helm-adaptive-mode t))

(use-package helm-descbinds
  :ensure t
  :bind (([remap describe-bindings] . helm-descbinds)))

(use-package helm-apropos
  :ensure helm
  :bind (([remap apropos-command] . helm-apropos)))

(use-package helm-command
  :ensure helm
  :bind (([remap execute-extended-command] . helm-M-x)
	 ("C-x C-m" . helm-M-x)))

(use-package helm-files
  :ensure helm
  :defer t
  :bind (([remap find-file] . helm-find-files)
	 ("C-c f r"         . helm-recentf))
  :config (progn
	    (setq helm-recentf-fuzzy-match t
		  helm-ff-file-name-history-use-recentf t)
            (define-key helm-read-file-map (kbd "<backspace>") 'helm-find-files-up-one-level)
            (define-key helm-find-files-map (kbd "<backspace>") 'helm-find-files-up-one-level)))

(use-package recentf
  :init (recentf-mode)
  :config (setq recentf-max-saved-items 200
		recentf-max-menu-items 15
		recentf-auto-cleanup 300
		recentf-exclude (list "/\\.git/.*\\'"
				      "/elpa/.*\\'")))

(use-package avy-jump
  :ensure avy
  :bind (("C-c j" . avy-goto-word-1)
	 ("C-c l" . avy-goto-line)
	 ("C-c n b" . avy-pop-mark)))

(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :config 
  (progn 
    (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
    (setq projectile-completion-system 'helm
	  projectile-find-dir-includes-top-level t))
  :diminish projectile-mode)

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package eyebrowse
  :ensure t
  :init (eyebrowse-mode))

(use-package neotree
  :ensure t
  :bind (("C-c f t" . neotree-toggle))
  :config (setq neo-window-width 32
		neo-create-file-auto-open t
		neo-banner-message nil
		neo-show-updir-line nil
		neo-mode-line-type 'neotree
		neo-smart-open t
		neo-dont-be-alone t
		neo-persist-show nil
		neo-show-hidden-files t
		neo-auto-indent-point t))

(use-package windmove
  :init (windmove-default-keybindings)
  :bind (("C-<tab>" . windmove-right)))

(setq indicate-empty-lines t
      require-final-newline t)

(use-package helm-ring
  :ensure helm
  :bind (([remap yank-pop] . helm-show-kill-ring)))

(use-package smartparens
  :ensure t
  :init (progn (require 'smartparens-config)
	       (smartparens-global-mode)
	       (show-smartparens-global-mode)
	       (dolist (hook '(inferior-emacs-lisp-mode-hook
			       emacs-lisp-mode-hook))
		 (add-hook hook #'smartparens-strict-mode)))
  :bind (("C-S-<left>" . sp-forward-barf-sexp)
	 ("C-S-<right>" . sp-forward-slurp-sexp)))

(use-package hl-line
  :config (progn
	    (set-face-attribute 'hl-line nil :background "#555")
	    (global-hl-line-mode 1)))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
	  (add-hook hook #'rainbow-delimiters-mode)))

(use-package web-mode
  :ensure t
  :defer t
  :mode "\\.\\(html\\|soy\\|jsx\\|xmb\\)\\'"
  :config (setq web-mode-markup-indent-offset 2))

(use-package magit
  :ensure t
  :init (progn (magit-wip-after-save-mode)
	       (magit-wip-after-apply-mode))
  :chords (("GG" . magit-status)))

(use-package git-timemachine
  :ensure t)

(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer)
	 ("C-x k" . bury-buffer)
	 ("C-c x k" . kill-this-buffer)))

(use-package apropospriate-theme
  :ensure t
  :config (load-theme 'apropospriate-dark))

(use-package smart-mode-line
  :ensure t
  :config (progn
	    (setq sml/no-confirm-load-theme t)
	    (setq sml/theme 'respectful))
  :init (sml/setup))

(use-package column-number-mode
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
	  (add-hook hook #'column-number-mode)))

(use-package menu-bar-mode
  :init (menu-bar-mode -1))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config (dolist (hook '(emacs-lisp-mode-hook
                          css-mode-hook
                          html-mode-hook
                          web-mode-hook))
            (add-hook hook #'rainbow-mode)))

(use-package helm-swoop
  :ensure t
  :after helm
  :bind (("C-c s s" . helm-swoop))
  :config (progn
            (setq helm-swoop-pre-input-function (lambda () "")
                  helm-swoop-speed-or-color t
                  helm-swoop-split-window-function #'helm-default-display-buffer)
            
            (defun ciniglio/helm-swoop-last-query ()
              "Use the last query as the input"
              (interactive)
              (with-selected-window (minibuffer-window)
                (delete-minibuffer-contents)
                (insert helm-swoop-last-query)))
            (bind-key "C-s" 'ciniglio/helm-swoop-last-query helm-swoop-map)))

(use-package focus-autosave-mode
  :ensure t
  :init (focus-autosave-mode 1)
  :diminish focus-autosave-mode)

(use-package volatile-highlights
  :ensure t
  :config (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)

(use-package sane-term
  :ensure t
  :init (progn
          (setq sane-term-shell-command "/bin/zsh")
          (add-hook 'term-mode-hook
                    (lambda()
                      (progn
                        (setq yas-dont-activate t
                              term-buffer-maximum-size 10000)
                        (defface term-color-black 
                          '((t (:foreground "#3f3f3f" :background "#272822"))) 
                          "Unhelpful docstring.")
                        (defface term-color-red
                          '((t (:foreground "#cc9393" :background "#272822"))) 
                          "Unhelpful docstring.")
                        (defface term-color-green
                          '((t (:foreground "#7f9f7f" :background "#272822"))) 
                          "Unhelpful docstring.")
                        (defface term-color-yellow
                          '((t (:foreground "#f0dfaf" :background "#272822"))) 
                          "Unhelpful docstring.")
                        (defface term-color-blue 
                          '((t (:foreground "#6d85ba" :background "#272822"))) 
                          "Unhelpful docstring.")
                        (defface term-color-magenta 
                          '((t (:foreground "#dc8cc3" :background "#272822"))) 
                          "Unhelpful docstring.")
                        (defface term-color-cyan
                          '((t (:foreground "#93e0e3" :background "#272822"))) 
                          "Unhelpful docstring.")
                        (defface term-color-white
                          '((t (:foreground "#dcdccc" :background "#272822"))) 
                          "Unhelpful docstring.")
                        '(term-default-fg-color ((t (:inherit term-color-white))))
                        '(term-default-bg-color ((t (:inherit term-color-black))))
                        
                        ;; ansi-term colors
                        (setq ansi-term-color-vector
                              [term term-color-black term-color-red term-color-green term-color-yellow 
                                    term-color-blue term-color-magenta term-color-cyan term-color-white])
                        (bind-key "M-m" 'my/sane-term term-mode-map)
                        (bind-key "M-m" 'my/sane-term term-raw-map))
)))
  :config (progn
            (defun my/sane-term (arg)
              (interactive "P")
              (if arg
                  (sane-term-create)
                (sane-term))))
  :bind (("M-m" . my/sane-term)))

(use-package aec-autosave :load-path "lisp/")

(use-package savehist
  :init (savehist-mode t)
  :config (setq savehist-save-minibuffer-history t
                savehist-autosave-interval 180))

(use-package which-key
  :ensure t
  :config (which-key-mode)
  :diminish which-key-mode)

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("M-3" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("M-4" . mc/mark-previous-like-this)))

(use-package compile
  :config (progn
            (setq compilation-scroll-output t)
            (defun aec-switch-to-compilation-buffer ()
              (interactive)
              (switch-to-buffer "*compilation*"))
            (bind-key "<f5>" (defhydra aec-compile (:color blue)
                               "Compilation"
                               ("b" aec-switch-to-compilation-buffer "buffer")
                               ("<f5>" recompile "recompile")))))

(use-package java-mode
  :defer t
  :init (add-hook 'java-mode-hook
                    (lambda ()
                      (subword-mode))))

(use-package delete-selection-mode
  :init (delete-selection-mode 1))

(use-package fringe
  :config (set-face-attribute 'fringe nil
                              :background (face-background 'default)))

(use-package org
  :ensure t)

(use-package helm-org-rifle
  :ensure t)

(use-package helm-orgcard
  :ensure t)
