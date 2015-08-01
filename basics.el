;; We don’t really need to garbage collect as frequently as Emacs would like to by default, so set the threshold up higher:
(setq gc-cons-threshold (* 100 1024 1024)) ;; 100 mb
;; Allow font-lock-mode to do background parsing
(setq jit-lock-stealth-time 1
      ;; jit-lock-stealth-load 200
      jit-lock-chunk-size 1000
      jit-lock-defer-time 0.05)

;; line-number-mode displays the current line number in the mode line,
;; however it stops doing that in buffers when encountering at least
;; one overly long line and displays two question marks instead. This
;; is pretty unhelpful, the only workaround I’ve been able to find was
;; to increase line-number-display-width to a substantially higher
;; value.
(setq line-number-display-limit-width 10000)

;; Scrolling improvements perhaps
(setq scroll-conservatively 10000
      scroll-preserve-screen-position t)

;; Recentf
(setq recentf-max-saved-items 300
      recentf-exclude '("/auto-install/" ".recentf" "/repos/" "/elpa/"
			"\\.mime-example" "\\.ido.last" "COMMIT_EDITMSG"
			".gz"
			"~$" "/tmp/" "/ssh:" "/sudo:" "/scp:")
      recentf-auto-cleanup 600)


;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; Shutdown obnoxious popup windows
(require 'popwin)
(popwin-mode 1)

(setq popwin:special-display-config
      (append
       '(("*Helm Find Files*" :height 10)
	 ("*undo-tree*" :width 0.2 :position right)
         ("^\*helm.+\*$" :regexp t :height 30 :position bottom))
       popwin:special-display-config))


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

;; highlight the current line
(global-hl-line-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; replace selection
(delete-selection-mode 1)

;; Helpful functions
(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

;; I do what I want
(put 'narrow-to-region 'disabled nil)
