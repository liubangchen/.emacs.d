;;; -*- lexical-binding: t; -*-
;; PlantUML and diagram related configurations

(use-package plantuml-mode
  :ensure t
  :mode ("\\.plu\\'" "\\.plantuml\\'" "\\.puml\\'" "\\.pu\\'")
  :config
  (setq plantuml-jar-path (expand-file-name "~/.emacs.d/javalibs/plantumllib/plantuml.jar")
        plantuml-default-exec-mode 'jar
        plantuml-java-options ""
        plantuml-output-type "svg"
        plantuml-options "-charset UTF-8"))

(add-hook 'plantuml-mode-hook
          (lambda ()
            ;; 禁用 corfu-mode（plantuml 不需要补全）
            (corfu-mode -1)
            ;; Execute plantuml-save-png function with C-c C-s
            (local-set-key (kbd "C-c C-s") 'plantuml-save-png)))

(use-package flycheck-plantuml
  :ensure t
  :commands (flycheck-plantuml-setup)
  :init
  (with-eval-after-load 'flycheck
    (flycheck-plantuml-setup)))

(use-package graphviz-dot-mode
  :ensure t
  :mode ("\\.dot\\'"))

(use-package gnuplot
  :mode "\\.gp\\'\\|\\.plot\\'"
  :commands gnuplot-make-buffer)

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
