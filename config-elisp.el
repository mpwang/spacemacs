;; (use-package semantic
;;   :init
;;   (setq semantic-default-submodes
;;         '(;; Perform semantic actions during idle time
;;           global-semantic-idle-scheduler-mode
;;           ;; Use a database of parsed tags
;;           global-semanticdb-minor-mode
;;           ;; Decorate buffers with additional semantic information
;;           global-semantic-decoration-mode
;;           ;; Highlight the name of the function you're currently in
;;           global-semantic-highlight-func-mode
;;           ;; show the name of the function at the top in a sticky
;;           global-semantic-stickyfunc-mode
;;           ;; Generate a summary of the current tag when idle
;;           global-semantic-idle-summary-mode
;;           ;; Show a breadcrumb of location during idle time
;;           global-semantic-idle-breadcrumbs-mode
;;           ;; Switch to recently changed tags with `semantic-mrub-switch-tags',
;;           ;; or `C-x B'
;;           global-semantic-mru-bookmark-mode))
;;   (add-hook 'emacs-lisp-mode-hook 'semantic-mode)
;;   )

(use-package minimap
  :custom
  (minimap-major-modes '(emacs-lisp-mode))
  :init
  (defface minimap-font-face
    '((default :family "Iosevka Term SS09" :height 30))
    "Face used for text in minimap buffer, notably the font family and height.
This height should be really small.  You probably want to use a
TrueType font for this.  After changing this, you should
recreate the minimap to avoid problems with recentering."
    :group 'minimap)
  (defface minimap-active-region-background
    '((((background dark)) (:background "#464646"))
      (t (:background "#C847D8FEFFFF")))
    "Face for the active region in the minimap.
By default, this is only a different background color."
    :group 'minimap)
  )
