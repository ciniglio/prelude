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

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

;;; Customization
(defconst aec-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit
  :defer t
  :config
  (setq custom-file aec-custom-file
        custom-buffer-done-kill nil            ; Kill when existing
        custom-buffer-verbose-help nil         ; Remove redundant help text
        ;; Show me the real variable name
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil)
  :init (load aec-custom-file 'no-error 'no-message))

;;; Autosave files
;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)

;; Scrolling
(setq scroll-conservatively 10000
      scroll-preserve-screen-position t)

(use-package ciniglio-site-lisp
  :if (file-exists-p "~/.ciniglio-site-specific/site-lisp")
  :load-path "~/.ciniglio-site-specific/site-lisp/")

(blink-cursor-mode 0)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t)
(fset 'yes-or-no-p #'y-or-n-p)

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
  :config
  (setq vc-follow-symlinks t))

(use-package helm
  :ensure t
  :bind (("C-c c b" . helm-resume))
  :init (helm-mode 1)
  :config (progn
	    (setq helm-split-window-in-side-p t)
	    (set-face-attribute 'helm-selection-line nil
				:background "brightmagenta")
	    (set-face-attribute 'helm-selection nil
				:background "brightmagenta")
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

(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default indent-tabs-mode nil)

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
  :bind (([remap isearch-forward] . helm-swoop)))

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
         (message "Using sane term")
         (setq sane-term-shell-command "/bin/zsh")
         (add-hook 'term-mode-hook
                   (lambda()
                     (setq yas-dont-activate t))))
  :config (progn
            (defun my/sane-term (arg)
              (interactive "P")
              (if arg
                  (sane-term-create)
                (sane-term))))
  :bind (("M-m" . my/sane-term)))

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defun prelude-auto-save-command ()
  "Save the current buffer."
  (when (and buffer-file-name
             (buffer-modified-p (current-buffer))
             (file-writable-p buffer-file-name))
    (save-buffer)))

(defmacro advise-commands (advice-name commands &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (before ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))

;; advise all window switching functions
(advise-commands "auto-save"
                 (switch-to-buffer other-window windmove-up windmove-down windmove-left windmove-right)
                 (prelude-auto-save-command))

(add-hook 'mouse-leave-buffer-hook 'prelude-auto-save-command)

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
         ("C-<" . mc/mark-previous-like-this)))

(use-package compile
  :config (setq compilation-scroll-output t))
