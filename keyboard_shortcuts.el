;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Multiple Cursors
(global-set-key (kbd "C-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Search
;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; Chords
(key-chord-mode 1)
(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "jk" 'ace-jump-buffer)
(key-chord-define-global "ww" 'ace-window)
(key-chord-define-global "GG" 'magit-status)

;; Navigation
(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-tab] (lambda ()
                            (interactive)
                            (other-window -1)))
(define-key magit-mode-map [C-tab] 'other-window)
(define-key magit-mode-map [C-S-tab] (lambda ()
                                       (interactive)
                                       (other-window -1)))

(global-set-key (kbd "C-c SPC") 'avy-goto-line)
