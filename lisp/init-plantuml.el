;;; -*- lexical-binding: t; -*-
(use-package plantuml-mode
  :ensure t
  :mode ("\\.plu\\'" "\\.plantuml\\'" "\\.puml\\'")
  :config
  (setq plantuml-jar-path (expand-file-name "~/.emacs.d/javalibs/plantumllib/plantuml.jar")
        plantuml-default-exec-mode 'jar))


(add-hook 'plantuml-mode-hook
          (lambda ()
            ;; 或禁用 corfu-mode
            (corfu-mode -1)
           ))
(use-package flycheck-plantuml
  :ensure t
  :commands (flycheck-plantuml-setup)
  :init
  (with-eval-after-load 'flycheck
    (flycheck-plantuml-setup)))

(use-package graphviz-dot-mode
  :ensure t
  :mode "\\.dot\\'")

(use-package gnuplot
  :mode "\\.gp\\'\\|\\.plot\\'"
  :commands gnuplot-make-buffer)

(use-package dockerfile-mode
  :ensure t
  :mode "/Dockerfile\\'")

(add-to-list 'auto-mode-alist '("\.pu$" . plantuml-mode))

;;(setq plantuml-jar-path
;;(expand-file-name "~/.emacs.d/javalibs/plantuml.jar"))

(setq plantuml-java-options "")
(setq plantuml-output-type "svg")
(setq plantuml-options "-charset UTF-8")

;; Execute plantuml-save-png function with C-c C-s at plantuml-mode
(add-hook 'plantuml-mode-hook
          (lambda () (local-set-key (kbd "C-c C-s") 'plantuml-save-png)))

(setq default-tab-width 2)
(add-to-list 'auto-mode-alist '("\.dot$" . graphviz-dot-mode))
;; If you want to save png file when saving .pu file, comment in here
;; (add-hook ‘plantuml-mode-hook
;; (lambda () (add-hook ‘after-save-hook ‘plantuml-save-png)))

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


;; Function to save plantuml as png
(defun plantuml-save-png ()
  (interactive)
  (when (buffer-modified-p)
    (map-y-or-n-p "Save this buffer before executing PlantUML?"
                  'save-buffer (list (current-buffer))))
  (let ((code (buffer-string))
        out-file
        cmd)
    (when (string-match "^\\s-*@startuml\\s-+\\(\\S-+\\)\\s*$" code)
      (setq out-file (match-string 1 code)))
    (setq cmd (concat
               "java -Djava.awt.headless=true -jar " plantuml-java-options " "
               (shell-quote-argument plantuml-jar-path) " "
               (and out-file (concat "-t" (file-name-extension out-file))) " "
               plantuml-options " "
               (buffer-file-name)))
    (message cmd)
    (call-process-shell-command cmd nil 0)))
(provide 'init-plantuml)
