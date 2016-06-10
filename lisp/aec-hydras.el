

(use-package hydra
  :ensure t)

(defhydra hydra-org-template (:color blue :hint nil)
    "
_C_enter  _q_uote    _c_lojure     _L_aTeX:
_l_atex   _e_xample  _s_cheme      _i_ndex:
_a_scii   _v_erse    _E_macs-lisp  _I_NCLUDE:
s_r_c     ^ ^        _p_ython      _H_TML:
_h_tml    ^ ^        ^ ^           _A_SCII:
"
    ("c" (hot-expand-and-edit "clojure"))
    ("s" (hot-expand-and-edit "scheme"))
    ("E" (hot-expand-and-edit "emacs-lisp"))
    ("p" (hot-expand-and-edit "python"))
    ("r" (hot-expand "<s"))
    ("e" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("C" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("o" nil "quit"))

(defun hot-expand (str)
  "Expand org template."
  (insert str)
  (org-try-structure-completion))

(defun hot-expand-and-edit (str)
  "Expand src template for given languange and enter org-edit-special."
  (hot-expand "<s")
  (insert str)
  (forward-line)
  (org-edit-special))

(define-key org-mode-map "<" (lambda () (interactive)
                               (if (bolp)
                                   (hydra-org-template/body)
                                 (self-insert-command 1))))

(defhydra aec-hydra-launcher (:color teal :columns 7)
  "Launcher"
  ("j"     bookmark-jump "bookmark jump")
  ("b"     bookmark-set "bookmark set")
  ("s"     helm-semantic-or-imenu "helm semantic")
  ("o"     org-capture "org capture")
  ("t"     my/sane-term "term")
  ("nt"    (my/sane-term 1) "new term")
  ("C-g"   nil "cancel" :color blue))

(bind-key "C-c SPC" #'aec-hydra-launcher/body)

(provide 'aec-hydras)
