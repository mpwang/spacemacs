(setq use-package-verbose t)
(use-package use-package-ensure-system-package
  :ensure t)

;;;;;;;;;;;;;;
;; keyboard ;;
;;;;;;;;;;;;;;

(when (eq system-type 'darwin)
  ;; Mac OS/custom keyboard
  ;; Make control key just under the thumb, perfect way to control emacs,
  ;; respect to original keyboard shortcut design of Emacs on Symbolics's lisp
  ;; machine keyboard(http://xahlee.info/kbd/keyboard_hardware_and_key_choices.html).
  ;; See also:
  ;; http://ergoemacs.org/emacs/emacs_kb_shortcuts_pain.html
  ;; http://ergoemacs.org/emacs/modernization_meta_key.html
  ;; http://ergoemacs.org/emacs/emacs_pinky.html

  ;; Command and Contrl are already swapped by karabiner-elements globally
  (setq mac-command-modifier 'meta)
  ;; Left option for hyper for personal key-bindings.
  (setq mac-option-modifier 'hyper)
  ;; Avoid Command + H to hide Emacs, but also prevent Command + Space from invoking change input method.
  (setq mac-pass-command-to-system nil)
  )

;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;

;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(defun sanityinc/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2016-01-08"
  (interactive)
  (let (
        (deactivate-mark nil)
        ξp1 ξp2)
    (if (use-region-p)
        (setq ξp1 (region-beginning)
              ξp2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alnum:]")
        (setq ξp1 (point))
        (skip-chars-forward "[:alnum:]")
        (setq ξp2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region ξp1 ξp2)
      (put this-command 'state 1))
     ((equal 1  (get this-command 'state))
      (upcase-region ξp1 ξp2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region ξp1 ξp2)
      (put this-command 'state 0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custome key bindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up
(global-set-key (kbd "M-c") 'xah-toggle-letter-case)
(global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)

;; remap default key binding
(global-set-key (kbd "C-0") 'spacemacs/delete-window)
(global-set-key (kbd "C-1") 'spacemacs/toggle-maximize-buffer)
(global-set-key (kbd "C-2") 'spacemacs/split-window-vertically-and-switch)
(global-set-key (kbd "C-3") 'spacemacs/split-window-horizontally-and-switch)

;; borrow from http://ergoemacs.org/emacs/emacs_isearch_by_arrow_keys.html
;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
(define-key isearch-mode-map (kbd "<left>") 'isearch-ring-retreat )
(define-key isearch-mode-map (kbd "<right>") 'isearch-ring-advance )
(define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward) ; single key, useful
(define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward) ; single key, useful


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editing configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable C-u C-SPC C-SPC ... to repeatly pop mark
(setq set-mark-command-repeat-pop t)

;; enable yank to replace selected text
(delete-selection-mode t)

;; prettify symbols everywhere
(global-prettify-symbols-mode)

(setq vc-follow-symlinks t)

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode)
  (dolist (item '(html-mode python-mode))
    (add-to-list 'aggressive-indent-excluded-modes item)))

(use-package shrink-whitespace
  :bind ("M-SPC" . shrink-whitespace))

(use-package switch-window
  :bind ("M-o" . switch-window))

;; spell checking using aspell
(use-package flyspell
  :ensure-system-package (aspell . aspell))

(use-package pangu-spacing
  :config
  (global-pangu-spacing-mode 1)
  ;; only insert real whitespace in some specific mode, but just add virtual space in other mode,
  (add-hook 'org-mode-hook
            '(lambda ()
               (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; display configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display-time-mode t)

;; configurations for emacs-plus installed by brew
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist
               '(ns-transparent-titlebar . nil))
  ;; dark frame title bar
  (add-to-list 'default-frame-alist
               '(ns-appearance . dark))
  )

;; theme
(use-package doom-themes
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  )

;; make visited file buffer different from special buffers
;; use together with doom-themes
(use-package solaire-mode
  :requires (doom-themes)
  :hook ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  :config
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (solaire-mode-swap-bg)
  (setq solaire-mode-remap-modeline nil)
  )

;; Chinese and English fonts alignment
(use-package cnfonts
  :config
  (cnfonts-enable)
  (setq cnfonts-use-face-font-rescale t)
  )

;; center buffers
(use-package perfect-margin
  :custom
  (perfect-margin-visible-width 128)
  :config
  (perfect-margin-mode t)
  )

(with-eval-after-load 'minimap
  (defadvice switch-window--list (after swtich-window--list-exclude-minimap nil activate)
    "Exclude minimap window from window list available to swtich."
    (setq ad-return-value
          (delq nil
                (mapcar (lambda (win)
                          (let ((name (buffer-name (window-buffer win))))
                            (if (string-match minimap-buffer-name name) nil win)))
                        ad-return-value))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable dired ls on MacOS
(use-package dired
  :ensure-system-package (gls . coreutils)
  :config
  (setq insert-directory-program "gls"
        dired-use-ls-dired t))

;; magit
(use-package magit
  :init
  (setq-default git-magit-status-fullscreen t))

(use-package markdown-mode
  :ensure-system-package markdown)

(use-package multiple-cursors
  :config
  (bind-key "C->" 'mc/mark-next-like-this)
  (bind-key "C-<" 'mc/mark-previous-like-this)
  )
