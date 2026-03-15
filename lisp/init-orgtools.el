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

(use-package valign
  :hook ((org-mode markdown-mode) . valign-mode)
  :config
  (setq valign-fancy-bar t))

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


;; --- 1. 动态资源扫描函数 ---
(defun tj3/tj-get-resource-ids ()
  "从当前 Buffer 的 'Resources' 节点下提取所有二级子节点的 :resource_id: 属性。"
  (save-excursion
    (goto-char (point-min))
    (let ((resources '()))
      ;; 1. 定位到一级标题 "Resources"
      (if (re-search-forward "^\\* +Resources" nil t)
          (let ((end (save-excursion (org-goto-sibling) (point)))) ; 确定 Resources 节点的范围
            (org-narrow-to-subtree)
            (goto-char (point-min))
            ;; 2. 扫描该范围内的所有二级节点 (用 ^** 开头)
            (while (re-search-forward "^\\*\\* +.*$" nil t)
              (let ((rid (org-entry-get (point) "resource_id")))
                (when rid
                  (push rid resources))))
            (widen))
        (message "警告：未找到 'Resources' 根节点"))
      (if resources
          (reverse (delete-dups resources))
        '("No_Resource_ID_Found")))))

;; --- 2. 动态任务 ID 扫描 (从 task-001 开始) ---
(defun tj3/tj-generate-next-task-id ()
  "扫描全文，找到最大的 task-### 并生成下一个。"
  (save-excursion
    (goto-char (point-min))
    (let ((max-id 0))
      ;; 搜索属性抽屉中的 ID 或正文中的 task-xxx
      (while (re-search-forward "task-\\([0-9]+\\)" nil t)
        (let ((num (string-to-number (match-string 1))))
          (when (> num max-id) (setq max-id num))))
      (format "task-%03d" (1+ max-id)))))

;; --- 3. 获取所有任务 ID (用于依赖选择) ---
(defun tj3/tj-get-all-task-ids ()
  "获取当前文件中所有以 task- 开头的 ID。"
  (let (ids)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "task-[0-9]+" nil t)
        (push (match-string 0) ids)))
    (delete-dups ids)))

;; --- 4. 综合交互函数 ---
(defun tj3/tj-quick-setup-task ()
  "一站式设置 TaskJuggler 任务。"
  (interactive)
  (let* ((current-resources (tj3/tj-get-resource-ids))
         (all-tasks (tj3/tj-get-all-task-ids))
         ;; 自动生成 ID
         (next-id (tj3/tj-generate-next-task-id))
         ;; 分配资源 (从标签节点动态获取)
         (allocate (completing-read "分配资源 (Allocate): " current-resources))
         ;; 负责人 (通常与资源一致)
         (responsible (completing-read "负责人 (Responsible): " current-resources nil nil allocate))
         ;; 依赖关系 (下拉选择已有的任务 ID)
         (depends (completing-read "依赖任务 (Depends, 可手动输入): " all-tasks))
         (effort (read-string "工作量 (effort, e.g. 2d, 4h): " "1d"))
         (complete (read-string "进度 (complete, 0-100): " "0"))
         (start (org-read-date nil nil nil "开始时间 (Start): "))
         (priority (read-string "优先级 (Priority, 1-1000): " "500")))

    ;; 写入属性
    (org-set-property "ID" next-id)
    (org-set-property "allocate" allocate)
    (org-set-property "responsible" responsible)
    (unless (string-empty-p depends)
      (org-set-property "depends" depends))
    (org-set-property "effort" effort)
    (org-set-property "complete" (format "%s%%" complete))
    (org-set-property "start" start)
    (org-set-property "priority" priority)

    (message "任务 %s 设置成功！资源: %s, 依赖: %s" next-id allocate depends)))

(provide 'init-orgtools)
