
(use-package org-web-tools)
(use-package htmlize :ensure t)
(use-package ox-reveal
  :init
  (setq org-reveal-mathjax t
        org-reveal-root "file:///Users/chenlong/.emacs.d/reveal.js"))

(require 'ox-reveal)
(require 'htmlize)


(defun cfw:sync-mac-calendar()
  "dsfsadfdsa"
  (interactive)
  (let* ((files (directory-files-recursively "~/Library/Calendars" ".*ics$")))
    (with-temp-buffer
      (mapc (lambda (f)
              (and (/= (nth 7 (file-attributes (expand-file-name f) 'string)) 0)
                   (time-less-p (time-subtract (current-time) (seconds-to-time 2592000)) (nth 5 (file-attributes (expand-file-name f) 'string)))
                   ;;(message "time:%s" (file-local-name  f))
                   (insert-file-contents (expand-file-name f)))
              ) files)
      (write-region (point-min) (point-max) (expand-file-name "~/.emacs.d/mac.ics"))
      )))

(defun cfw:open-mac-calendar()
  ""
  (interactive)
  (cfw:open-ical-calendar (expand-file-name "~/.emacs.d/mac.ics")))


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


(use-package org-mind-map
  :init
  (require 'ox-org)
  :ensure t
  ;; Uncomment the below if 'ensure-system-packages` is installed
  ;;:ensure-system-package (gvgen . graphviz)
  :config
  (add-to-list 'org-mind-map-default-graph-attribs '("dpi" . "350"))
  (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )

(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
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

(defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
   (pcase org-journal-file-type
     (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
     (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
     (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
     (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))

(defun org-journal-save-entry-and-exit()
  "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/GTD/journal"
        org-journal-file-format "%Y-%m-%d"
        org-journal-file-type 'monthly
        org-journal-file-header 'org-journal-file-header-func
        org-journal-date-format "%Y-%m-%d %A"))

(setq org-ellipsis "???"
      ;; ???, ???, ???, ???, , ???, ???, ???, ???
      org-deadline-warning-days 7
      org-default-notes-file "~/GTD/inbox.org"
      org-directory "~/GTD/"
      org-capture-templates
      '(
        ("t" "????????????" entry (file+olp "~/GTD/inbox.org" "Tasks" "????????????")
         "* TODO %?  \n " :empty-lines 1)
        ("a" "???????????????" entry (file+olp "~/GTD/inbox.org" "Demand" "???????????????")
         "* TODO %?  \n " :empty-lines 1)
        ("n" "????????????" entry (file+olp "~/GTD/inbox.org" "Notes" "????????????")
         "*  %?  \n " :empty-lines 1)
        ("n" "????????????" entry (file+olp "~/GTD/inbox.org" "Meetings" "????????????")
         "*  %?  \n " :empty-lines 1)
        ("j" "Journal entry" entry (file+datetree "~/GTD/journal/journal.org")
         "* %U - %^{heading}\n  %?")
        ("p" "????????????" entry (file+olp "~/GTD/personal.org" "Tasks" "????????????")
         "*  %?  \n " :empty-lines 1)
        )
      ;;?????????????????????
      org-agenda-files
      (list "~/GTD/inbox.org"
            "~/GTD/personal.org"
            "~/GTD/doing.org"
            "~/GTD/finished.org"))


;; ????????????????????????????????????????????????????????????
(custom-set-variables
 '(org-refile-targets
   (quote
    (("inbox.org" :level . 1) ("doing.org" :level . 1) ("personal.org" :level . 1) ("finished.org":level . 1))
    )))

;; ! : ?????????????????????????????????????????????
;; @ : ?????????????????????????????????????????????
;; ??????????????????@???!,??????@/!
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!/@)" "DOING(d@/!)" "PENDDING(p@/!)" "DONE(o@)" "CANCELED(c@/!)")))
(setq org-log-done 'note)
;;????????????
(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("STARTED" . "yellow") ("DOING" . "green4") ("PENDDING" . "OrangeRed") ("DONE" . "#32cd32")
        ("CANCELED" . (:foreground "blue" :weight bold))))

;;????????????
(setq system-time-locale "zh_CN")
(custom-set-variables
 '(org-display-custom-times t)
 '(org-time-stamp-custom-formats (quote ("<%Y-%m-%d %A>" . "<%Y %m %d  %A [%H:%M]>"))))
(format-time-string "%Y-%m-%d %A")

;;(use-package org-habit)

;;(use-package org-habit)
;;??????????????????
(use-package org-super-agenda
  :after org-agenda
  :hook (org-agenda-mode . org-super-agenda-mode)
  :config
  (setq org-super-agenda-groups
        '((:name "Today"  ; Optionally specify section name
           :time-grid t  ; Items that appear on the time grid
           :todo "TODAY")  ; Items that have this TODO keyword
          (:name "Important"
           ;; Single arguments given alone
           ;;:tag "bills"
           :priority "A")
          (:order-multi (2 (:name "Shopping in town"
                            ;; Boolean AND group matches items that match all subgroups
                            :and (:tag "shopping" :tag "@town"))
                           (:name "Food-related"
                            ;; Multiple args given in list with implicit OR
                            :tag ("food" "dinner"))
                           (:name "Personal"
                            ;;:habit t
                            :tag "personal")
                           (:name "Space-related (non-moon-or-planet-related)"
                            ;; Regexps match case-insensitively on the entire entry
                            :and (:regexp ("space" "NASA")
                                  ;; Boolean NOT also has implicit OR between selectors
                                  :not (:regexp "moon" :tag "planet")))))
          (:name "Scheduled earlier "
           :scheduled past))))

(setq org-agenda-time-grid '((daily today require-timed) "----------------------" nil)
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-include-diary t
      org-agenda-block-separator t
      org-agenda-compact-blocks t
      org-agenda-start-with-log-mode t)


;;(setq org-bullets-bullet-list '( "???" "???" "???" "???" "???"))
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
;;??????
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (plantuml . t)
   (ditaa . t)
   (gnuplot . t)
   (dot . t)))

(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(setq org-ditaa-jar-path
      (expand-file-name "~/.emacs.d/javalibs/ditaa0_9.jar"))

(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/javalibs/plantuml.jar"))
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

(provide 'init-orgtools)
