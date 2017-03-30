;;; Functions for interacting with buffers

(defun aec/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (car (aec/get-unshown-buffers))))

(defun aec/get-unshown-buffers ()
  (interactive)
  (let* ((buffers (buffer-list (selected-frame)))
        (non-special-buffers (remove-if (lambda (buffer)
                                          (or ;; (string-prefix-p "*" (buffer-name buffer))
                                              (string-prefix-p " *" (buffer-name buffer))))
                                        buffers))
        (buffers-that-arent-shown (remove-if (lambda (buffer)
                                               (get-buffer-window buffer (selected-frame)))
                                             non-special-buffers)))
    buffers-that-arent-shown))

(provide 'aec-buffers)
