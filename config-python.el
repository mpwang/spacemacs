;;;;;;;;;;;;;;;;;;;;;
;; Python language ;;
;;;;;;;;;;;;;;;;;;;;;
;; 设置 python 解析器路径
(setq python-shell-interpreter "/usr/local/bin/python3")
(setq python-indent-offset 4)

(use-package ob-ipython
  :ensure-system-package ((jupyter . "pip3 install jupyter")))

(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages '(ipython . t))
  (add-to-list 'org-babel-load-languages '(python . t))

  ;; 不再询问是否允许执行代码块
  (setq org-confirm-babel-evaluate nil)
  ;; display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  )
