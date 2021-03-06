(defun mark-region-lines (start end)
  "mark region by line region"
  (interactive "nStart Line: \nnEnd Line:")
  (when (>= end start)
    (goto-line start)
    (goto-char (line-beginning-position))
    (set-mark (point))
    (goto-line end)
    (goto-char (line-end-position))
    )
  (when (< end start)
    (message "End line must > start line")
    )
  )

(defun md2pdf ()
  "Generate pdf from currently open markdown."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (shell-command-to-string
     (concat "pandoc "
             filename
             " -o "
             (file-name-sans-extension filename)
             ".pdf -V mainfont=IPAPGothic -V fontsize=16pt --pdf-engine=lualatex"))
    (shell-command-to-string
     (concat "xdg-open "
             (file-name-sans-extension filename)
             ".pdf"))))


(defun md2docx ()
  "Generate docx from currently open markdown."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (shell-command-to-string
     (concat "pandoc "
             filename
             " -t docx -o "
             (file-name-sans-extension filename)
             ".docx -V mainfont=IPAPGothic -V fontsize=16pt --toc --highlight-style=zenburn"))
    (shell-command-to-string
     (concat "xdg-open "
             (file-name-sans-extension filename)
             ".docx"))))

(use-package thrift
  :ensure t
  :defer t)


(use-package log4j-mode
  :ensure t
  :defer t)

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'go-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'python-mode-hook         'hs-minor-mode)

(provide 'init-function)
