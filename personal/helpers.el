(defun unique-name-for-buffer-p (new-name)
  (let ((file-name (buffer-file-name))
        (dir-name (file-name-directory buffer-file-name)))
    (let ((new-complete-name (concat dir-name new-name)))
      (progn (not (string-equal file-name new-complete-name))))))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name))
        (dir-name (file-name-directory (buffer-file-name))))
    (cond ((not filename) 
           (message "Buffer '%s' is not visiting a file!" name))
          ((not (unique-name-for-buffer-p new-name)) 
           (message "A buffer named '%s' already exists in that location!" new-name))
          (t (rename-file (file-name-nondirectory filename) new-name 1)
             (kill-buffer)
             (set-buffer (find-file (concat dir-name new-name)))))))


(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()  
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (or (minibufferp)
          (string-match "Minibuf" (buffer-name)))
      (ido-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))




