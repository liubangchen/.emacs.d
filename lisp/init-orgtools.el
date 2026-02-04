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
(setq org-plantuml-exec-mode 'plantuml)
(setq org-plantuml-executable-path "~/.emacs.d/javalibs/plantuml")
(setq org-plantuml-executable-args '("-headless" "-charset UTF-8"))

(setq org-confirm-babel-evaluate nil)

;;(use-package valign
;;  :hook (org-mode . valign-mode)
;;  :config
;;  (setq valign-fancy-bar t))

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
;; brew install pngpaste imagemagick
(defvar org-screenshot-dir "~/notes/org/notes/images/"
  "保存截图的目录，会自动创建")

(defun org-screenshot ()
  "截图→保存→插入 org 链接"
  (interactive)
  (unless (file-directory-p org-screenshot-dir)
    (make-directory org-screenshot-dir t))
  (let* ((basename (format-time-string "%Y-%m-%d_%H-%M-%S.png"))
         (path (expand-file-name basename org-screenshot-dir)))
    (cond
     ((eq system-type 'gnu/linux)
      (call-process "import" nil nil nil path)) ; ImageMagick
     ((eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" path)))
    (insert (format "[[file:%s]]" path))
    (org-display-inline-images)))

(defun org-clip-image ()
  "剪贴板有图片时保存并插入链接"
  (interactive)
  (let* ((basename (format-time-string "%Y-%m-%d_%H-%M-%S.png"))
         (path (expand-file-name basename org-screenshot-dir)))
    (unless (file-directory-p org-screenshot-dir) (make-directory org-screenshot-dir t))
    (cond
     ((eq system-type 'gnu/linux)
      (call-process "xclip" nil nil nil "-selection" "clipboard" "-t" "image/png" "-o" path))
     ((eq system-type 'darwin)
      (call-process "pngpaste" nil nil nil path))
     ((eq system-type 'windows-nt)
      (w32-read-png-from-clipboard path)))
    (insert (format "[[file:%s]]" path))
    (org-display-inline-images)))

(defun org-clip-image-current ()
  "剪贴板有图片时保存并插入链接"
  (interactive)
  (let* ((basename (format-time-string "%Y-%m-%d_%H-%M-%S.png"))
         (basedir (when buffer-file-name
                    (file-name-directory buffer-file-name)))
         (imagedir (expand-file-name "images" basedir))
         (path (expand-file-name basename imagedir)))
    (unless (file-directory-p imagedir) (make-directory imagedir t))
    (cond
     ((eq system-type 'gnu/linux)
      (call-process "xclip" nil nil nil "-selection" "clipboard" "-t" "image/png" "-o" path))
     ((eq system-type 'darwin)
      (call-process "pngpaste" nil nil nil path))
     ((eq system-type 'windows-nt)
      (w32-read-png-from-clipboard path)))
    (insert (format "[[file:%s]]" path))
    (org-display-inline-images)))


;; 这是 GTD 的核心：把 inbox 的条目移动到 gtd.org 的具体项目下
(setq org-refile-targets '(("idea.org" :maxlevel . 3)
                           ("gtd.org" :level . 1)
                           ("book.org" :level . 1)
                           ("note.org" :level . 1)
                           ("journal.org" :maxlevel . 2)))
;;以此种方式显示路径： gtd.org/Project A/Subtask
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil) ; 可以在一个 prompt 里完成路径补全

(setq org-log-into-drawer t)
(setq org-log-done 'time)

(use-package org-super-agenda
  :ensure t
  :after org
  :init
  (org-super-agenda-mode 1)
  :config
  (setq org-super-agenda-header-separator "\n"
        org-super-agenda-header-prefix " "
        org-super-agenda-unmatched-name "📂 其他杂项")
  (setq org-agenda-custom-commands
        '(("a" "任务看板"
           ((agenda "" ((org-agenda-span 'day)  ; 只看今天的日程
                        (org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "🔥 逾期任务"
                            :deadline past
                            :order 1)
                           (:name "🎯 今日重点"
                            :and (:deadline today :scheduled today)
                            :order 2)
                           (:name "⏳ 正在进行"
                            :todo "STARTED"
                            :order 3)
                           (:name "🔄 习惯养成"
                            :habit t
                            :order 4)
                           (:name "📅 即将到来"
                            :deadline future
                            :order 5)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "🏷️ 工作项目"
                             :tag "work"
                             :order 1)
                            (:name "🏠 生活琐事"
                             :tag "home"
                             :order 2)
                            (:name "📦 待办池"
                             :todo "TODO"
                             :order 3)
                            (:name "⏳ 等待他人"
                             :todo "WAITING"
                             :order 4)
                            (:discard (:tag ("archive" "ignore"))))))))))))

(custom-set-faces
 '(org-super-agenda-header ((t (:inherit org-modern-label :height 1.2 :weight bold :foreground "#51afef")))))

;; 让 Agenda 占据整个窗口，心无旁骛
(setq org-agenda-window-setup 'current-window)


(defun insert-org-common-header (&optional force)
  "插入富途 ES 内核升级故障汇报的专用 Org 文件头。
可通过 M-x 运行，若通过 C-u 触发则强制在开头插入。"
  (interactive "P")
  (if (and (not force) (not (= (buffer-size) 0)))
      (message "Buffer is not empty, use C-u M-x to force insert.")
    (save-excursion
      (goto-char (point-min))
      (insert "# -*- org -*-\n"
              "#+STARTUP: indent\n"
              "#+LaTeX_CLASS: article\n"
              "#+LATEX_CLASS_OPTIONS: [a4paper]\n"
              "#+STARTUP: showlevels 3\n"

              "#+Title: \n"
              "#+OPTIONS:author: liubangchen\n"
              "#+OPTIONS:date:nil\n"
              "#+LaTeX_HEADER: \\usepackage{ctex}\\ctexset{space=false}\\usepackage{listings}\n"
              "#+LATEX_HEADER: \\usepackage[left=2.5cm,right=2.5cm,top=2.5cm,bottom=2.5cm]{geometry}\n"
              "#+LaTeX_HEADER: \\linespread{1.2}\n"
              "#+LaTeX_HEADER: \\usepackage{xcolor}\n"
              "#+LaTeX_HEADER: \\usepackage{tabularx} \n"
              "#+LaTeX_HEADER: \\usepackage{colortbl}\n"
              "#+LaTeX_HEADER: \\usepackage{makecell}\n"
              "#+LATEX_HEADER: \\usepackage{adjustbox}\n"
              "# \\usepackage{adjustbox}\\usepackage{booktabs}\\usepackage{colortbl}\\usepackage{siunitx}\n"
              "#+LaTeX_HEADER: \\usepackage{multirow}\\usepackage{fontawesome}\n"
              "#+LaTeX_HEADER: \\usepackage{multicol}\n"
              "#+LaTeX_HEADER: \\usepackage{fontspec}\\usepackage{xltabular}\n"
              "#+BIND: org-latex-default-packages-alist nil\n\n"
              "#+LATEX_HEADER: \\usepackage{titlesec}\n"
              "#+LATEX_HEADER: \\titleformat{\\section}{\\Large\\bfseries\\color{blue}}{\\thesection}{1em}{}[\\titlerule]\n\n"
              "#+LATEX_HEADER: \\usepackage{xeCJK}\n"
              "#+LATEX_HEADER: \\usepackage{fontspec}\n"
              "#+LATEX_HEADER: \\setmainfont{PingFang SC}\n"
              "#+LATEX_HEADER: \\setCJKmainfont{PingFang SC}\n\n"
              "#+LATEX_HEADER: \\usepackage{fancyhdr}\n"
              "#+LATEX_HEADER: \\usepackage{graphicx}\n"
              "#+LATEX_HEADER: \\pagestyle{fancy}\n"
              "#+LATEX_HEADER: \\fancyhf{} % 清除所有默认设置\n"
              "#+LATEX_HEADER: \\lhead{\\includegraphics[width=2cm]{logo.png}} % 左页眉\n"
              "#+LATEX_HEADER: \\chead{} % 中页眉\n"
              "#+LATEX_HEADER: \\rhead{\\thepage} % 右页眉，显示页码\n"
              "#+LATEX_HEADER: \\lfoot{} % 左页脚\n"
              "#+LATEX_HEADER: \\renewcommand{\\headrulewidth}{0.4pt} % 页眉下划线\n"
              "\n\n")
      (message "Common Org header inserted.")))
  (goto-char (point-max)))

(provide 'init-orgtools)
