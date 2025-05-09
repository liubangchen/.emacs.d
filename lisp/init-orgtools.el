;;; -*- lexical-binding: t; -*-
(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in `org-mode` . "
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(set-time-zone-rule "Asia/Shanghai")

(defun org-journal-save-entry-and-exit()
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

(use-package org-journal
  :ensure t
  :defer t
  :init
  :config
  (setq org-journal-dir "~/notes/org/gtd/journal"
        org-journal-file-format "%Y-%m-%d"
        org-journal-file-type 'monthly
        org-journal-date-format "%Y-%m-%d %A"))

(setq system-time-locale "zh_CN")
(custom-set-variables
 '(org-display-custom-times t)
 '(org-time-stamp-custom-formats (quote ("[%Y-%m-%d %A]" . "[%Y %m %d  %A [%H:%M]]"))))

(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(setq org-ditaa-jar-path
      (expand-file-name "~/.emacs.d/javalibs/ditaa0_9.jar"))

(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/javalibs/plantumllib/plantuml.jar"))
(setq org-plantuml-exec-mode 'jar)
;;(setq org-plantuml-exec-mode 'plantuml)
;;(setq org-plantuml-executable-path "~/.emacs.d/javalibs/plantuml")
(setq org-plantuml-executable-args '("-headless" "-charset UTF-8"))
(setq org-confirm-babel-evaluate nil)

;;(defun org-screenshot ()
;;  "Take a screenshot into a time stamped unique-named file in the
;;same directory as the org-buffer and insert a link to this file."
;;  (interactive)
;;  (setq filename
;;        (concat
;;         (make-temp-name
;;          (concat "images\/ss"
;;                  "_"
;;                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
;;  (call-process "screencapture" nil nil nil "-i" filename)
;;  (insert (concat "[[./" filename "]]")))

;;(setq org-babel-python-command "python3")


(provide 'init-orgtools)
