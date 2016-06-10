(defvar *aec-idle-abort* nil)

(defun aec-run-when-idle-in
    (seconds f)
  (lexical-let ((seconds-to-idle 5)
                (f f))
    (run-at-time
     (format "%d sec" seconds)
     nil
     (lambda ()
       (progn
         (run-with-idle-timer
          seconds-to-idle
          nil
          (lambda ()
            (progn
             (message "Running function now...")
             (funcall f)))))))))

(defun aec-run-then-reschedule-in
    (seconds f &optional run-first)
  (progn
   (if run-first
       (progn
         (message "Running function immediately...")
         (funcall f)))
   (unless *aec-idle-abort*
     (lexical-let ((seconds-between-runs seconds)
                   (f f))
       (message "Rescheduling...")
       (aec-run-when-idle-in
        seconds-between-runs
        (lambda ()
          (aec-run-then-reschedule-in seconds-between-runs f t)))))))

(provide 'aec-idle)
