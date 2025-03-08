
;;(use-package org-web-tools)
;;(use-package htmlize :ensure t)
;;(use-package ox-reveal
;;  :init
;;  (setq org-reveal-mathjax t
;;        org-reveal-root "file:///Users/chenlong/.emacs.d/reveal.js"))

;;(require 'ox-reveal)
;;(require 'htmlize)


;;(defun cfw:sync-mac-calendar()
;;  "dsfsadfdsa"
;;  (interactive)
;;  (let* ((files (directory-files-recursively "~/Library/Calendars" ".*ics$")))
;;    (with-temp-buffer
;;      (mapc (lambda (f)
;;              (and (/= (nth 7 (file-attributes (expand-file-name f) 'string)) 0)
;;                   (time-less-p (time-subtract (current-time) (seconds-to-time 2592000)) (nth 5 (file-attributes (expand-file-name f) 'string)))
;;                   ;;(message "time:%s" (file-local-name  f))
;;                   (insert-file-contents (expand-file-name f)))
;;              ) files)
;;      (write-region (point-min) (point-max) (expand-file-name "~/.emacs.d/mac.ics"))
;;      )))
;;
;;(defun cfw:open-mac-calendar()
;;  ""
;;  (interactive)
;;  (cfw:open-ical-calendar (expand-file-name "~/.emacs.d/mac.ics")))

;;;
(use-package org-download
  :ensure t
  :config
  ;; add support to dired
  (setq org-download-image-dir "~/Downloads/images")
  (add-hook 'dired-mode-hook 'org-download-enable))

(setq org-html-htmlize-output-type nil)
(define-advice org-html-src-block (:override (src-block _contents info) pygmentize)
  "Highlight src-block via Pygmentize."
  (let ((lang (org-element-property :language src-block))
        (code (org-html-format-code src-block info)))
    (with-temp-buffer
      (call-process-region code nil "pygmentize" nil t nil "-l" lang "-f" "html")
      (buffer-string))))


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

;;画图
;;(org-babel-do-load-languages
;; 'org-babel-load-languages
;; '(
;;   (plantuml . t)
;;   (ditaa . t)
;;   (gnuplot . t)
;;   (julia . t)
;;   (python . t)
;;   (jupyter . t)
;;   (dot . t)))

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

(defun org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat "images\/ss"
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "screencapture" nil nil nil "-i" filename)
  (insert (concat "[[./" filename "]]")))

(setq org-babel-python-command "python3")

(require 'org-modern-indent)
(add-hook 'org-mode-hook #'org-modern-indent-mode 90)

;;(require 'org-pretty-table)
;;(add-hook 'org-mode-hook (lambda () (org-pretty-table-mode)))
(provide 'init-orgtools)
