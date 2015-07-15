(setq my-credentials-file "~/.emacs.d/.private.el")

(unless (file-exists-p "~/.emacs.d/.private.el")
  (display-warning 'init "Missing credentials file .private.el"))

(defun my-nickserv-password (_)
  (with-temp-buffer
    (insert-file-contents-literally my-credentials-file)
    (plist-get (read (buffer-string)) :nickserv-password)))

(defun erc-settings ()
  "Settings of `erc'."
  (require 'erc-highlight-nicknames)

  (add-to-list 'erc-modules 'highlight-nicknames)
  (erc-update-modules)

  (setq erc-nick-uniquifier "0"))

(eval-after-load "erc"
  `(erc-settings))

(require 'erc-hl-nicks)

;; (require 'erc-highlight-nicknames)
;; (add-to-list 'erc-modules 'highlight-nicknames)
;; (erc-update-modules)



;; (setq circe-network-options
;;       '(("Freenode"
;;          :nick "_alejandro"
;;          :channels ("#emacs" "#emacs-circe"
;;                     "#clojure" "#ansible"
;;                     "#reactjs")
;;          :nickserv-password my-nickserv-password)))
