;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'org
  ;; do not display inline image by default
  (setq org-startup-with-inline-images nil)
  ;; disable interpret "_" and "^" for export
  (setq org-export-with-sub-superscripts nil)

  (dolist (item '(("p" . "src ipython")
                  ("el" . "src emacs-lisp")))
    (add-to-list 'org-structure-template-alist item))
  )
