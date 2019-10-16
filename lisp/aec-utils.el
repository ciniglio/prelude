
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun aec/beginning-of-word ()
  "Move point to the beginning of nearest word"
  (interactive)
  (forward-word)
  (backward-word))

(defun aec/end-of-word-p (curpos)
  "whether the point is at the end of a word."
  (interactive "d")
  (or
   ;; end of buffer is obviously the end of the word
   (equal curpos (point-max))

   ;; word character followed by non-underscore non-word character
   (and
    (equal (string-match "\\w" (substring (buffer-string) (- curpos 2))) 0)
    (and
     (equal (string-match "\\W" (substring (buffer-string) (- curpos 1))) 0)
     (not (equal (string-match "_" (substring (buffer-string) (- curpos 1))) 0))))

   ;; underscore followed by non-word character
   (and
    (equal (string-match "_" (substring (buffer-string) (- curpos 2))) 0)
    (equal (string-match "\\W" (substring (buffer-string) (- curpos 1))) 0))))

(defun snake-case-ify ()
  "Take a camelCased word and transform to snake_case"
  (interactive)
  (aec/beginning-of-word)
  (while (not (aec/end-of-word-p (point)))
    (call-interactively 'subword-downcase)
    (insert "_"))
  (delete-char -1))

(defun aec/copy-file-name-as-kill ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(provide 'aec-utils)
