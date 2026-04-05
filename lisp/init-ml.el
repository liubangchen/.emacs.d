;;; init-ml.el --- AI/ML tools configuration -*- lexical-binding: t -*-
;;
;;
;;; code

(setq python-interpreter "~/.pyenv/shims/python3")
(setq python-shell-interpreter "~/.pyenv/shims/python3")

(use-package aidermacs
  :bind (("C-c M-a" . aidermacs-transient-menu))
  :config
  (setq aidermacs-auto-commits nil
        aidermacs-exit-kills-buffer t
        aidermacs-extra-args '("--thinking-tokens" "128k")
        aidermacs-backend 'vterm))

(use-package gterm
  ;; 使用 cxa 的 fork（PR #4），修复 ghostty 1.3.2+ 编译问题
  ;; TODO: 主仓库合并后切回 rwc9u/emacs-libgterm
  :vc (:url "https://github.com/cxa/emacs-libgterm" :branch "main")
  :commands gterm
  :init
  (setq gterm-always-compile-module t)
  :config
  (setq gterm-shell "/bin/zsh")
  ;; 修复中文输入：gterm-mode-map 只绑定了 ASCII 32-126，
  ;; CJK 等非 ASCII 字符事件没有 keymap 绑定，fallback 到
  ;; self-insert-command，但 buffer 是 read-only 所以被忽略。
  ;; 用 remap 将所有 self-insert-command 重定向到 gterm-send-key，
  ;; gterm-send-key 通过 this-command-keys-vector 获取实际按键字符
  ;; 并发送到终端进程，天然支持任何 Unicode 字符。
  (define-key gterm-mode-map [remap self-insert-command] #'gterm-send-key)

  ;; 确保光标在 gterm buffer 中始终可见
  (add-hook 'gterm-mode-hook
            (lambda ()
              (setq-local cursor-type 'bar))))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (unless (executable-find "claude")
    (setq claude-code-ide-cli-path "claude-internal"))
  (claude-code-ide-emacs-tools-setup)

  ;; @file 快速引用功能
  (defun my/claude-code-ide--send-at-files (files)
    "将 FILES 列表以 @path 格式发送到 Claude Code 终端。
路径相对于项目根目录，让终端显示更简洁。"
    (let ((buffer-name (claude-code-ide--get-buffer-name)))
      (if-let ((buffer (get-buffer buffer-name)))
          (let* ((root (or (when-let ((proj (project-current)))
                             (project-root proj))
                           default-directory))
                 (text (mapconcat
                        (lambda (f)
                          (concat "@" (file-relative-name f root)))
                        files " ")))
            (with-current-buffer buffer
              (claude-code-ide--terminal-send-string text))
            (claude-code-ide-debug "Sent @file references: %s" text))
        (user-error "当前项目没有 Claude Code 会话"))))

  (defun my/claude-code-ide--select-files (candidates prompt)
    "用文件名展示 CANDIDATES 供选择，返回选中的完整路径列表。
PROMPT 为提示文字。显示时用文件名，选中后映射回完整路径。"
    (let* ((name-to-paths (make-hash-table :test 'equal))
           ;; 构建 display-name -> paths 映射，重名文件加目录区分
           (_ (dolist (f candidates)
                (let ((name (file-name-nondirectory f)))
                  (puthash name (cons f (gethash name name-to-paths)) name-to-paths))))
           (display-alist
            (cl-loop for f in candidates
                     for name = (file-name-nondirectory f)
                     collect (cons (if (> (length (gethash name name-to-paths)) 1)
                                      ;; 重名：显示 filename<parent-dir>
                                      (format "%s<%s>" name
                                              (file-name-nondirectory
                                               (directory-file-name (file-name-directory f))))
                                    name)
                                  f)))
           (display-names (mapcar #'car display-alist))
           (selected (completing-read-multiple prompt display-names nil t)))
      (delq nil (mapcar (lambda (s) (cdr (assoc s display-alist))) selected))))

  (defun my/claude-code-ide-at-buffer ()
    "选择已打开的 buffer 文件，@ 引用到 Claude Code 终端。"
    (interactive)
    (let* ((candidates (cl-loop for buf in (buffer-list)
                                for file = (buffer-file-name buf)
                                when file collect file))
           (selected (my/claude-code-ide--select-files candidates "选择 buffer: ")))
      (if selected
          (my/claude-code-ide--send-at-files selected)
        (user-error "未选择任何文件"))))

  (defun my/claude-code-ide-at-project-file ()
    "选择项目文件，@ 引用到 Claude Code 终端。"
    (interactive)
    (let* ((proj (project-current t))
           (candidates (when proj (project-files proj)))
           (selected (when candidates
                       (my/claude-code-ide--select-files candidates "选择文件: "))))
      (if selected
          (my/claude-code-ide--send-at-files selected)
        (user-error "未选择任何文件"))))

  (defun my/claude-code-ide-paste-image ()
    "将剪贴板中的图片保存为临时文件，以 @path 发送到 Claude Code 终端。
macOS 需要 pngpaste，Linux 需要 xclip。"
    (interactive)
    (let* ((tmp-dir (expand-file-name "claude-images" temporary-file-directory))
           (tmp-file (expand-file-name
                      (format "clipboard-%s.png" (format-time-string "%Y%m%d-%H%M%S"))
                      tmp-dir))
           (cmd (cond
                 ((executable-find "pngpaste") (list "pngpaste" tmp-file))
                 ((executable-find "xclip")
                  (list "sh" "-c" (format "xclip -selection clipboard -t image/png -o > %s"
                                          (shell-quote-argument tmp-file))))
                 (t (user-error "需要安装 pngpaste (macOS) 或 xclip (Linux)")))))
      (make-directory tmp-dir t)
      (if (zerop (apply #'call-process (car cmd) nil nil nil (cdr cmd)))
          (if (and (file-exists-p tmp-file) (> (file-attribute-size (file-attributes tmp-file)) 0))
              (my/claude-code-ide--send-at-files (list tmp-file))
            (user-error "剪贴板中没有图片"))
        (user-error "从剪贴板提取图片失败"))))

  (with-eval-after-load 'claude-code-ide-transient
    (transient-append-suffix 'claude-code-ide-menu '(0 2 -1)
      '("@" "@ buffer file" my/claude-code-ide-at-buffer))
    (transient-append-suffix 'claude-code-ide-menu '(0 2 -1)
      '("f" "@ project file" my/claude-code-ide-at-project-file))
    (transient-append-suffix 'claude-code-ide-menu '(0 2 -1)
      '("I" "Paste image" my/claude-code-ide-paste-image))))

(provide 'init-ml)
