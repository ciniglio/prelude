(defun applescript-quote-string (argument)
  "Quote a string for passing as a string to AppleScript."
  (if (or (not argument) (string-equal argument ""))
      "\"\""
    ;; Quote using double quotes, but escape any existing quotes or
    ;; backslashes in the argument with backslashes.
    (let ((result "")
          (start 0)
          end)
      (save-match-data
        (if (or (null (string-match "[^\"\\]" argument))
                (< (match-end 0) (length argument)))
            (while (string-match "[\"\\]" argument start)
              (setq end (match-beginning 0)
                    result (concat result (substring argument start end)
                                   "\\" (substring argument end (1+ end)))
                    start (1+ end))))
        (concat "\"" result (substring argument start) "\"")))))


(defun send-region-to-omnifocus (beg end)
  "Send the selected region to OmniFocus.
Use the first line of the region as the task name and the second
and subsequent lines as the task note."
  (interactive "r")
  (let* ((region (buffer-substring-no-properties beg end))
         (match (string-match "^\\(.*\\)$" region))
         (name (substring region (match-beginning 1) (match-end 1)))
         (note (if (< (match-end 0) (length region))
                   (concat (substring region (+ (match-end 0) 1) nil) "\n\n")
                 "")))
    (do-applescript
     (format "set theDate to current date
              set taskName to %s
              set taskNote to %s
              set taskNote to (taskNote) & \"Added from Emacs on \" & (theDate as string)
              tell front document of application \"OmniFocus\"
                make new inbox task with properties {name:(taskName), note:(taskNote)}
              end tell"
             (applescript-quote-string name)
             (applescript-quote-string note)))))

(provide 'aec-apple)
