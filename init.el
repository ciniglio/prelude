(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

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
  :bind (("C-c h r" . helm-resume)
         ("C-c h i" . helm-semantic-or-imenu))
  :init (helm-mode 1)
  :config (progn
	    (setq helm-split-window-in-side-p t)
	    (set-face-attribute 'helm-selection-line nil
				:background "#ba55d3")
	    (set-face-attribute 'helm-selection nil
				:background "#ba55d3")
            (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
            (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
            (define-key helm-map (kbd "C-z") 'helm-select-action)
            (define-key helm-map (kbd "M-n") 'helm-next-source)
            (define-key helm-map (kbd "M-p") 'helm-previous-source))
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

(use-package magit
  :ensure t
  :init (progn (magit-wip-after-save-mode)
	       (magit-wip-after-apply-mode))
  :chords (("GG" . magit-status)))

(use-package git-timemachine
  :ensure t)

(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :config 
  (progn 
    (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
    (setq projectile-completion-system 'helm
	  projectile-find-dir-includes-top-level t
          projectile-switch-project-action 'magit-status))
  :diminish projectile-mode)

(use-package helm-projectile
  :demand
  :ensure t
  :init (progn
            (add-to-list 'helm-mini-default-sources 'helm-source-projectile-recentf-list)
            (add-to-list 'helm-mini-default-sources 'helm-source-projectile-buffers-list))
  :bind (([remap projectile-find-file] . helm-projectile-find-file)))

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

(use-package helm-ring
  :ensure helm
  :bind (([remap yank-pop] . helm-show-kill-ring)))

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
  :bind (("C-<tab>" . other-window)))

(setq indicate-empty-lines t
      require-final-newline t)

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

(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer)
	 ("C-x k" . bury-buffer)
	 ("C-c x k" . kill-this-buffer)))

(use-package apropospriate-theme
  :ensure t
  :config  (load-theme 'apropospriate-dark))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (run-at-time "1 sec" nil
                         (lambda ()
                           (set-face-attribute 'fringe nil
                                               :background (face-background 'default))))))

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

(menu-bar-mode -1)

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config (dolist (hook '(emacs-lisp-mode-hook
                          css-mode-hook
                          html-mode-hook
                          web-mode-hook))
            (add-hook hook #'rainbow-mode)))

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
                        (bind-key "M-m" 'my/sane-term term-raw-map)
                        (bind-key "s-k" 'erase-buffer term-mode-map)
                        (bind-key "s-k" 'erase-buffer term-raw-map))
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
  :config (progn
            (setq which-key-idle-delay 1.0
                  which-key-idle-secondary-delay 0.0)
            (which-key-mode))
  :diminish which-key-mode)

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("M-3" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("M-4" . mc/mark-previous-like-this))
  :bind (:map mc/keymap
              ("<return>" . multiple-cursors-mode)
              ("RET" . multiple-cursors-mode)))

(use-package compile
  :config (progn
            (setq compilation-scroll-output t)
            (defun aec-switch-to-compilation-buffer ()
              (interactive)
              (switch-to-buffer "*compilation*"))
            (bind-key "<f5>"
                      (defhydra aec-compile (:color blue)
                        "Compilation"
                        ("b" aec-switch-to-compilation-buffer "buffer")
                        ("<f5>" recompile "recompile")))))

(use-package java-mode
  :defer t
  :init (add-hook 'java-mode-hook (lambda () (subword-mode))))

(delete-selection-mode 1)

(use-package org-plus-contrib
  :ensure t
  :pin org
  :config (setq org-replace-disputed-keys t
                org-startup-folded nil
                org-startup-truncated nil))

(use-package helm-org-rifle
  :ensure t)

(use-package helm-orgcard
  :ensure t)

(use-package bm
  :ensure t
  :bind (("C-c b" . hydra-bm/body))
  :config
  (progn
    (setq-default bm-buffer-persistence t) ; buffer persistence on by default

    (when bm-buffer-persistence
      (setq bm-repository-file (locate-user-emacs-file "bm-repository"))

      ;; Load bm repository
      (when (file-exists-p bm-repository-file)
        (bm-repository-load))

      ;; Saving bookmarks
      (add-hook 'kill-buffer-hook #'bm-buffer-save)
      ;; Saving the repository to file when on exit.
      ;; kill-buffer-hook is not called when Emacs is killed, so we
      ;; must save all bookmarks first.
      (defun aec/bm-save-all-bm-to-repository ()
        (bm-buffer-save-all)
        (bm-repository-save))
      (add-hook 'kill-emacs-hook #'aec/bm-save-all-bm-to-repository)
      (add-hook 'after-save-hook #'bm-buffer-save)
      ;; The `after-save-hook' is not necessary to use to achieve persistence,
      ;; but it makes the bookmark data in repository more in sync with the file
      ;; state.

      ;; Restoring bookmarks
      (add-hook 'find-file-hooks   #'bm-buffer-restore)
      (add-hook 'after-revert-hook #'bm-buffer-restore)
      ;; The `after-revert-hook' is not necessary to use to achieve persistence,
      ;; but it makes the bookmark data in repository more in sync with the file
      ;; state. This hook might cause trouble when using packages
      ;; that automatically reverts the buffer (like vc after a check-in).
      ;; This can easily be avoided if the package provides a hook that is
      ;; called before the buffer is reverted (like `vc-before-checkin-hook').
      ;; Then new bookmarks can be saved before the buffer is reverted.
      ;; Make sure bookmarks is saved before check-in (and revert-buffer)
      (add-hook 'vc-before-checkin-hook #'bm-buffer-save))

    (when (display-graphic-p) ; Add fringe only if display is graphic (GUI)
      (define-fringe-bitmap 'bm-marker-left [#xF8    ; ▮ ▮ ▮ ▮ ▮ 0 0 0
                                             #xFC    ; ▮ ▮ ▮ ▮ ▮ ▮ 0 0
                                             #xFE    ; ▮ ▮ ▮ ▮ ▮ ▮ ▮ 0
                                             #x0F    ; 0 0 0 0 ▮ ▮ ▮ ▮
                                             #x0F    ; 0 0 0 0 ▮ ▮ ▮ ▮
                                             #xFE    ; ▮ ▮ ▮ ▮ ▮ ▮ ▮ 0
                                             #xFC    ; ▮ ▮ ▮ ▮ ▮ ▮ 0 0
                                             #xF8])) ; ▮ ▮ ▮ ▮ ▮ 0 0 0

    (setq bm-highlight-style 'bm-highlight-only-fringe)
    (setq bm-cycle-all-buffers t) ; search all open buffers for bookmarks

    (defun aec/bm-bookmark-regexp ()
      (interactive)
      (if (use-region-p)
          (progn
            (bm-bookmark-regexp-region (region-beginning) (region-end))
            (deactivate-mark))
        (bm-bookmark-regexp)))

    (defhydra hydra-bm (:color pink
                        :hint nil
                        :body-pre (when (not (use-region-p)) (push-mark)))
      "
Bookmark _n_ext (_N_ in lifo order)            toggle book_m_ark        ^^_/_ bm lines matching regexp                          toggle per_s_istence                 _l_ist bookmarks
         _p_revious (_P_ in lifo order)        _a_nnotate               _x_/_X_ remove all bm from current/all buffer(s)        _r_eturn to from where you started
    "
      ("m"   bm-toggle)
      ("M"   bm-toggle :color blue)
      ("a"   bm-bookmark-annotate :color blue)
      ("n"   bm-common-next)
      ("N"   bm-lifo-next)
      ("p"   bm-common-previous)
      ("P"   bm-lifo-previous)
      ("/"   aec/bm-bookmark-regexp :color blue)
      ("s"   bm-toggle-buffer-persistence)
      ("x"   bm-remove-all-current-buffer :color blue)
      ("X"   bm-remove-all-all-buffers :color blue)
      ("r"   pop-to-mark-command :color blue)
      ("l"   helm-bm :color blue)
      ("RET" nil "cancel" :color blue)
      ("q"   nil "cancel" :color blue))))

(use-package helm-bm
  :ensure t)

(use-package aec-utils :load-path "lisp/")

(use-package aec-idle
  :load-path "lisp/"
  :config (aec-run-then-reschedule-in 600 'recentf-save-list))
(put 'erase-buffer 'disabled nil)

(use-package undo-tree
  :ensure t
  :bind (([remap undo] . undo-tree-visualize)))

(use-package rcirc
  :config (progn (setq rcirc-server-alist
                       '(("irc.freenode.net"
                          :channels ("##programming" "#clojure" "#ocaml" "#lobsters" "#emacs"))
                         ("irc.mozilla.org"
                          :channels ("#rust-beginners" "#rust"))))
                 (add-hook 'rcirc-mode-hook (lambda () (rcirc-omit-mode)))))

(use-package aec-buffers
  :load-path "lisp/"
  :bind (("s-b" . aec/switch-to-previous-buffer )))

(use-package tuareg
  :ensure t)

(use-package merlin
  :ensure t
  :init (progn
          (add-hook 'tuareg-mode-hook 'merlin-mode)
          (with-eval-after-load 'company
            (add-to-list 'company-backends 'merlin-company-backend))
          (add-hook 'merlin-mode-hook 'company-mode)))
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
