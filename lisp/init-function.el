;;; -*- lexical-binding: t; -*-
;;(use-package ivy
;;   :defer t
;;   :config
;;   (add-to-list 'ivy-format-functions-alist '(t . ivy-format-function-arrow)))

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
             "-V geometry:a4paper,margin=2cm "
             filename
             " -o "
             (file-name-sans-extension filename)
             ".pdf --pdf-engine=xelatex --variable mainfont=\"微软雅黑\" --variable sansfont=\"微软雅黑\""))
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

;;(use-package gitconfig-mode
;;  :ensure t
;;  :defer t)

;;(use-package gitignore-mode
;;  :ensure t
;;  :defer t)

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun nxml-pretty-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
    (nxml-mode)
    (indent-region begin end)))

(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'go-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'python-mode-hook         'hs-minor-mode)

(setq org-html-scripts "<![CDATA[/*><!--*/
 function CodeHighlightOn(elem, id)
 {
   var target = document.getElementById(id);
   if(null != target) {
     elem.cacheClassElem = elem.className;
     elem.cacheClassTarget = target.className;
     target.className = \"code-highlighted\";
     elem.className   = \"code-highlighted\";
   }
 }
 function CodeHighlightOff(elem, id)
 {
   var target = document.getElementById(id);
   if(elem.cacheClassElem)
     elem.className = elem.cacheClassElem;
   if(elem.cacheClassTarget)
     target.className = elem.cacheClassTarget;
 }
/*]]>*///-->
</script>")

(defun lb/html2org-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (kill-new (shell-command-to-string "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' | pandoc -f html -t json | pandoc -f json -t org | sed 's/ / /g'"))
  (yank))

;;pip install -U yapf
;;pip install futures
(defun lsp-python-ms-format-buffer ()
  (interactive)
  (when (and (executable-find "yapf") buffer-file-name)
    (call-process "yapf" nil nil nil "-i" buffer-file-name)))
(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'lsp-python-ms-format-buffer t t)))
;;(add-hook 'python-mode-hook 'yapf-mode)

(provide 'init-function)
