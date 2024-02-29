
;;(use-package org-web-tools)
;;(use-package htmlize :ensure t)
;;(use-package ox-reveal
;;  :init
;;  (setq org-reveal-mathjax t
;;        org-reveal-root "file:///Users/chenlong/.emacs.d/reveal.js"))

;;(require 'ox-reveal)
;;(require 'htmlize)


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
  (setq org-journal-dir "~/notes/org/gtd/journal"
        org-journal-file-format "%Y-%m-%d"
        org-journal-file-type 'monthly
        org-journal-file-header 'org-journal-file-header-func
        org-journal-date-format "%Y-%m-%d %A"))

;;(setq org-ellipsis "⤵"
;;      ;; ➡, ⚡, ▼, ↴, , ∞, ⬎, ⤷, ⤵
;;      org-deadline-warning-days 7
;;      org-default-notes-file "~/notes/org/gtd/inbox.org"
;;      org-directory "~/notes/org/gtd/"
;;      org-capture-templates
;;      '(
;;        ("t" "待办任务" entry (file+olp "~/notes/org/gtd/inbox.org" "Tasks" "待办任务")
;;         "* TODO %?  \n " :empty-lines 1)
;;        ("x" "临时需求表" entry (file+olp "~/notes/org/gtd/inbox.org" "Demand" "临时需求表")
;;         "* TODO %?  \n " :empty-lines 1)
;;        ("n" "笔记待办" entry (file+olp "~/notes/org/gtd/inbox.org" "Notes" "笔记待办")
;;         "*  %?  \n " :empty-lines 1)
;;        ("m" "会议安排" entry (file+olp "~/notes/org/gtd/inbox.org" "Meetings" "会议安排")
;;         "*  %?  \n " :empty-lines 1)
;;        ("j" "Journal entry" entry (file+datetree "~/notes/org/gtd/journal/journal.org")
;;         "* %U - %^{heading}\n  %?")
;;        ("p" "个人待办" entry (file+olp "~/notes/org/gtd/personal.org" "Tasks" "个人待办")
;;         "*  %?  \n " :empty-lines 1)
;;        )
;;      ;;显示他们的内容
;;      org-agenda-files
;;      (list "~/notes/org/gtd/inbox.org"
;;            "~/notes/org/gtd/personal.org"))


;; 将项目转接在各文件之间，方便清理和回顾。
;;(custom-set-variables
;; '(org-refile-targets
;;   (quote
;;    (("inbox.org" :level . 1) ("doing.org" :level . 1) ("personal.org" :level . 1) ("finished.org":level . 1))
;;    )))

;; ! : 切换到该状态时会自动添加时间戳
;; @ : 切换到该状态时要求输入文字说明
;; 如果同时设定@和!,使用@/!
;;(setq org-todo-keywords
;;     '((sequence "待办(t)" "开始(s!/@)" "进行中(d@/!)" "阻塞(p@/!)" "完成(o@)" "取消(c@/!)")))
;;(setq org-log-done 'note)
;;颜色设置
;;(setq org-todo-keyword-faces
;;      '(("TODO" . org-warning) ("开始" . "yellow") ("进行中" . "green4") ("阻塞" . "OrangeRed") ("完成" . "#32cd32")
;;        ("CANCELED" . (:foreground "blue" :weight bold))))
;;
;;日期设置
(setq system-time-locale "zh_CN")
(custom-set-variables
 '(org-display-custom-times t)
 '(org-time-stamp-custom-formats (quote ("[%Y-%m-%d %A]" . "[%Y %m %d  %A [%H:%M]]"))))
(format-time-string "%Y-%m-%d %A")

;;(use-package org-habit)

;;(use-package org-habit)
;;日历面板设置
;;(use-package org-super-agenda
;;  :after org-agenda
;;  :hook (org-agenda-mode . org-super-agenda-mode)
;;  :config
;;  (setq org-super-agenda-groups
;;        '((:name "Today"  ; Optionally specify section name
;;           :time-grid t  ; Items that appear on the time grid
;;           :todo "TODAY")  ; Items that have this TODO keyword
;;          (:name "Important"
;;           ;; Single arguments given alone
;;           ;;:tag "bills"
;;           :priority "A")
;;          (:order-multi (2 (:name "Shopping in town"
;;                            ;; Boolean AND group matches items that match all subgroups
;;                            :and (:tag "shopping" :tag "@town"))
;;                           (:name "Food-related"
;;                            ;; Multiple args given in list with implicit OR
;;                            :tag ("food" "dinner"))
;;                           (:name "Personal"
;;                            ;;:habit t
;;                            :tag "personal")
;;                           (:name "Space-related (non-moon-or-planet-related)"
;;                            ;; Regexps match case-insensitively on the entire entry
;;                            :and (:regexp ("space" "NASA")
;;                                  ;; Boolean NOT also has implicit OR between selectors
;;                                  :not (:regexp "moon" :tag "planet")))))
;;          (:name "Scheduled earlier "
;;           :scheduled past))))

;;(setq org-agenda-time-grid '((daily today require-timed) "----------------------" nil)
;;      org-agenda-skip-scheduled-if-done t
;;      org-agenda-skip-deadline-if-done t
;;      org-agenda-include-deadlines t
;;      org-agenda-include-diary t
;;      org-agenda-block-separator t
;;      org-agenda-compact-blocks t
;;      org-agenda-start-with-log-mode t)

(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
  (org-ellipsis "⤵")
  :hook (org-mode . org-bullets-mode))

(use-package org-pretty-tags
  :demand t
  :config
   (setq org-pretty-tags-surrogate-strings
         (quote
          (("TOPIC" . "☆")
           ("PROJEKT" . "💡")
           ("SERVICE" . "✍")
           ("Blog" . "✍")
           ("music" . "♬")
           ("security" . "🔥"))))
   (org-pretty-tags-global-mode))

(use-package org-fancy-priorities
  :diminish
  :demand t
  :defines org-fancy-priorities-list
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (unless (char-displayable-p ?❗)
    (setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL"))))

;;(setq org-bullets-bullet-list '( "⦿" "○" "✸" "✿" "◆"))
;;(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
;;画图
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (plantuml . t)
   (ditaa . t)
   (gnuplot . t)
   (julia . t)
   (python . t)
   (jupyter . t)
   (dot . t)))

(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(setq org-ditaa-jar-path
      (expand-file-name "~/.emacs.d/javalibs/ditaa0_9.jar"))

;;(setq org-plantuml-jar-path
;;      (expand-file-name "~/.emacs.d/javalibs/plantumllib/plantuml.jar"))
(setq org-plantuml-exec-mode 'plantuml)
(setq org-plantuml-executable-path "~/.emacs.d/javalibs/plantuml")
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
(provide 'init-orgtools)
