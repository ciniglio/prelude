(setq my-credentials-file "~/.emacs.d/.private.el")
(setq circe-reduce-lurker-spam t)
(enable-circe-color-nicks)

(unless (file-exists-p "~/.emacs.d/.private.el")
  (display-warning 'init "Missing credentials file .private.el"))

(defun my-nickserv-password (_)
  (with-temp-buffer
    (insert-file-contents-literally my-credentials-file)
    (plist-get (read (buffer-string)) :nickserv-password)))

(setq circe-network-options
      '(("Freenode"
         :nick "_alejandro"
         :channels ("#emacs" "#emacs-circe"
                    "#clojure" "#ansible"
                    "#reactjs")
         :nickserv-password my-nickserv-password)))
