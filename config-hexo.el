
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customization for using hexo-renderer-org to generate hexo static web pages ;;
;;                                                                             ;;
;; using themes/next https://theme-next.org/                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun hexo-theme-next/link-add-font-awesome (text backend info)
  "Add fa-external-link class to <a> element."
  (let ((fa-external-element "<i class='fa fa-external-link' aria-hidden='true'></i></a>"))
    (when (and (eq backend 'hexo-html)
               (not (string-match-p fa-external-element text)))
      (replace-regexp-in-string "</a>" (concat " " fa-external-element) text)
      )))

(defun hexo-theme-next/center-quote (text backend info)
  "Transcode a CENTER-BLOCK from org to hexo theme/next centerquote tag plugin."
  (let ((blockquote-tag-start "<blockquote class='blockquote-center'>")
        (blockqoute-tag-end "</blockquote>"))
    (when (and (eq backend 'hexo-html)
               (not (string-match-p blockquote-tag-start text)))
      (let* ((result text)
             (result (replace-regexp-in-string "<div class=\"org-center\">" blockquote-tag-start result))
             (result (replace-regexp-in-string "</div>" blockqoute-tag-end result)))
        result))))

(with-eval-after-load 'ox
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; disable interpret "_" and "^" for export ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq org-export-with-sub-superscripts nil)

  (add-to-list 'org-export-filter-link-functions #'hexo-theme-next/link-add-font-awesome)
  (add-to-list 'org-export-filter-center-block-functions #'hexo-theme-next/center-quote)
  )

(with-eval-after-load 'ox-html
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; todo keyword use theme/next label CSS class ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq org-html-todo-kwd-class-prefix "label warning ")

  (setq org-html-postamble-format
        '(("en"
           "<p style='font-size: .7em; font-style: italic'>Generated using %c</p>")
          ))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; generate TOC using hexo theme/next tabs plugin at the top of article ;;
  ;;                                                                      ;;
  ;; redefine org-html-toc                                                ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun org-html-toc (depth info &optional scope)
    "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is
a plist used as a communication channel.  Optional argument SCOPE
is an element defining the scope of the table.  Return the table
of contents as a string, or nil if it is empty."
    (let ((toc-entries
	         (mapcar (lambda (headline)
		                 (cons (org-html--format-toc-headline headline info)
			                     (org-export-get-relative-level headline info)))
		               (org-export-collect-headlines info depth scope))))
      (when toc-entries
        (let ((toc (concat "<ul class=\"nav-tabs\">\n"
                           (hexo-theme-next/toc-nav-tabs toc-entries)
                           "</ul>"
                           "<div class=\"tab-content\">\n"
			                     (hexo-theme-next/toc-text toc-entries)
                           "</div>\n"
                           )))
	        (if scope toc
            (concat  "<div class=\"tabs\" id=\"org-toc\" style=\"font-size: 12px;\">\n"
		                 toc
		                 "</div>\n" ))
          ))))

  (defun hexo-theme-next/get-href (headline)
    (if (string-match "href=\"#\\(.*\\)\"" headline)
        (match-string-no-properties 1 headline)
      ""))

  (defun hexo-theme-next/toc-nav-tabs (toc-entries)
    (mapconcat
     (lambda (entry)
	     (let ((headline (car entry))
	           (level (cdr entry)))
         (when (= level 1)
           (let ((ahref (hexo-theme-next/get-href headline)))
             (concat "<li class=\"tab\">\n"
                     (replace-regexp-in-string
                      "<a href"
                      "<a class=\"btn\" href"
                      (replace-regexp-in-string ahref (concat "tab-" ahref) headline))
                     "</li>\n")))))
     toc-entries ""))

  (defun hexo-theme-next/toc-text (toc-entries)
    "Return innards of a table of contents, as a string.
TOC-ENTRIES is an alist where key is an entry title, as a string,
and value is its relative level, as an integer."
    (let* ((prev-level (1- (cdar toc-entries)))
	         (start-level prev-level))
      (concat
       (mapconcat
        (lambda (entry)
	        (let ((headline (car entry))
	              (level (cdr entry)))
	          (let ((ahref (hexo-theme-next/get-href headline )))
              (concat
	             (let* ((cnt (- level prev-level))
		                  (times (if (> cnt 0) (1- cnt) (- cnt))))
	               (setq prev-level level)
	               (if (> cnt 0)
                     (cond ((= level 1)
                            (format "<div class=\"tab-pane\" id=\"%s\">\n<ul>\n"
                                    (concat "tab-" ahref)))
                           ((> level 1)
                            "<li>\n"))
                   (cond ((= level 1)
                          (format "</ul>\n</div>\n<div class=\"tab-pane\" id=\"%s\">\n<ul>\n"
                                  (concat "tab-" ahref)))
                         ((> level 1)
                          "</li>\n<li>\n"))))
	             headline))))
        toc-entries "")
       "</div>"
       )))
  )
