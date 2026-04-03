;;; init-ml.el --- AI/ML tools configuration -*- lexical-binding: t -*-
;;
;;
;;; code

(use-package minuet
  :init
  ;; 如需启用自动补全
  ;; 注意：即使不启用 minuet-auto-suggestion-mode，也可以手动触发补全
  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
  :bind
  ;; use completion-in-region for completion
  ("M-y" . minuet-completion-region)
  ("M-p" . minuet-previous-suggestion) ;; invoke completion or cycle to next completion
  ("M-n" . minuet-next-suggestion) ;; invoke completion or cycle to previous completion
  ("M-A" . minuet-accept-suggestion) ;; accept whole completion
  ("M-a" . minuet-accept-suggestion-line) ;; accept current line completion
  ("M-e" . minuet-dismiss-suggestion)

  :config
  (setq minuet-provider 'openai-fim-compatible)
  (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:11434/v1/completions")
  ;; an arbitrary non-null environment variable as placeholder
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
  (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:32b"))


(setq python-interpreter "~/.pyenv/shims/python3")
(setq python-shell-interpreter "~/.pyenv/shims/python3")

(use-package aidermacs
  :bind (("C-c M-a" . aidermacs-transient-menu))
  :config
  (setq aidermacs-auto-commits nil
        aidermacs-exit-kills-buffer t
        aidermacs-extra-args '("--thinking-tokens" "128k")
        aidermacs-backend 'vterm))

(use-package ellama
  :ensure t
  :init
  ;; 设定默认使用的本地模型 (确保你已经在终端运行过 `ollama pull zephyr`)
  (setopt ellama-language "Chinese") ;; 让它尽量用中文回答
  (require 'llm-ollama)
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "qwen3-coder:30b" :embedding-model "pull granite-embedding:latest")))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :config
  ;; Fix: `make-directory' signals "File exists" when ~/.claude/ide/ already
  ;; exists on some Emacs builds.  Guard the call so it only runs when the
  ;; directory is truly absent.
  (defun my/claude-ide-mkdir-safe (orig-fn port project-dir)
    "Call ORIG-FN only creating the lockfile dir when it doesn't exist yet."
    (let ((dir (expand-file-name "~/.claude/ide/")))
      (unless (file-directory-p dir)
        (make-directory dir t)))
    (cl-letf (((symbol-function 'make-directory) #'ignore))
      (funcall orig-fn port project-dir)))
  (advice-add 'claude-code-ide-mcp--create-lockfile
              :around #'my/claude-ide-mkdir-safe)

  ;; 修复 C-x 1 无法关闭 claude side window 的问题
  ;; claude-code-ide 给 side window 设置了 (no-delete-other-windows . t)，
  ;; 导致 delete-other-windows 永远跳过它。
  ;; 在窗口创建后清除该参数即可恢复 C-x 1 的正常行为。
  (defun my/claude-ide-fix-delete-window (orig-fn buffer)
    "Advice: 创建 side window 后移除 no-delete-other-windows 参数。"
    (let ((window (funcall orig-fn buffer)))
      (when (and window (window-live-p window))
        (set-window-parameter window 'no-delete-other-windows nil))
      window))

  (advice-add 'claude-code-ide--display-buffer-in-side-window
              :around #'my/claude-ide-fix-delete-window)

  (setq claude-code-ide-cli-path
        (or (executable-find "claude")
            (executable-find "claude-internal")
            "claude"))
  (setq claude-code-ide-terminal-backend 'vterm)
  (setq claude-code-ide-enable-mcp-server t)
  (claude-code-ide-emacs-tools-setup)

  ;; ── 切换到 claude vterm buffer 时自动滚动到底部 ──────────
  ;;
  ;; 问题：claude-code-ide 为防闪烁设置了
  ;;   (setq-local vterm-scroll-to-bottom-on-output nil)
  ;; 导致从其他 buffer 切回 claude session 时，窗口停在顶部。
  ;;
  ;; 方案：advice display-buffer-in-side-window 之后，把 window-point
  ;; 移到 point-max 并 recenter 到底部。

  (defun my/claude-ide-scroll-to-bottom (orig-fn buffer)
    "Display BUFFER then scroll to bottom for vterm-backed sessions."
    (let ((window (funcall orig-fn buffer)))
      (when (and window
                 (buffer-live-p buffer)
                 (eq (buffer-local-value 'major-mode buffer) 'vterm-mode))
        (with-selected-window window
          (goto-char (point-max))
          (recenter -1)))
      window))

  (advice-add 'claude-code-ide--display-buffer-in-side-window
              :around #'my/claude-ide-scroll-to-bottom)

  ;; 通过 switch-buffer / consult-buffer / C-x o 切到 claude session 时自动滚底
  ;;
  ;; 修复：旧方案用 window-buffer-change-functions 且无条件执行，导致：
  ;; 1. 用户手动向上滚动阅读时被反复拉回底部
  ;; 2. C-x 1 执行期间触发 recenter 干扰窗口管理
  ;;
  ;; 新方案：
  ;; - 用 window-selection-change-functions（切换窗口）和
  ;;   window-buffer-change-functions（同窗口换 buffer）双钩子
  ;; - 跟踪 prev-buffer，同一 buffer 只滚动一次
  ;; - 忽略 minibuffer，避免 M-x / C-x b 往返时误触发
  ;; - condition-case 保护，不干扰 delete-other-windows 等操作

  (defvar my/claude-ide--prev-buffer nil
    "上次在 selected window 中看到的非 minibuffer buffer。
用于检测实际的 buffer/window 切换，避免重复滚动。")

  (defun my/claude-ide--maybe-scroll-claude (&rest _)
    "切换到 claude session buffer 时滚动到底部（每次进入只触发一次）。"
    (condition-case nil
        (let* ((win (selected-window))
               (buf (window-buffer win)))
          (when (and (not (minibufferp buf))
                     (not (eq buf my/claude-ide--prev-buffer)))
            (setq my/claude-ide--prev-buffer buf)
            (when (and (window-live-p win)
                       (buffer-live-p buf)
                       (with-current-buffer buf
                         (and (derived-mode-p 'vterm-mode)
                              (claude-code-ide--session-buffer-p buf))))
              (set-window-point win (point-max))
              (with-selected-window win
                (recenter -1)))))
      (error nil)))

  (add-hook 'window-selection-change-functions
            #'my/claude-ide--maybe-scroll-claude)
  (add-hook 'window-buffer-change-functions
            #'my/claude-ide--maybe-scroll-claude)

  ;; ── 自定义 MCP 工具 ──────────────────────────────────────

  ;; 1. 获取当前 buffer 的诊断信息 (flycheck/flymake errors & warnings)
  (defun my/mcp-get-buffer-diagnostics (file_path)
    "Get flycheck/flymake diagnostics for FILE_PATH."
    (claude-code-ide-mcp-server-with-session-context nil
      (let ((buf (or (find-buffer-visiting file_path)
                     (find-file-noselect file_path))))
        (with-current-buffer buf
          (let ((diags (claude-code-ide-diagnostics-get-all buf)))
            (if (> (length diags) 0)
                (json-encode diags)
              (format "No diagnostics for %s" file_path)))))))

  (claude-code-ide-make-tool
   :function #'my/mcp-get-buffer-diagnostics
   :name "get_buffer_diagnostics"
   :description "Get flycheck/flymake diagnostics (errors, warnings) for a file that is open in Emacs. Useful for understanding compilation errors and lint warnings."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file")))

  ;; 2. 获取当前打开的 buffer 列表
  (defun my/mcp-list-open-buffers ()
    "List all file-visiting buffers with their major modes."
    (claude-code-ide-mcp-server-with-session-context nil
      (let ((results '()))
        (dolist (buf (buffer-list))
          (when-let ((file (buffer-file-name buf)))
            (push (format "%s [%s]%s"
                          file
                          (with-current-buffer buf
                            (symbol-name major-mode))
                          (if (buffer-modified-p buf) " (modified)" ""))
                  results)))
        (if results
            (string-join (nreverse results) "\n")
          "No file-visiting buffers open"))))

  (claude-code-ide-make-tool
   :function #'my/mcp-list-open-buffers
   :name "list_open_buffers"
   :description "List all files currently open in Emacs with their major modes and modification status. Helps understand what the user is working on."
   :args nil)

  ;; 3. 获取 buffer 中光标位置上下文
  (defun my/mcp-get-cursor-context (file_path &optional lines_before lines_after)
    "Get code around the cursor position in FILE_PATH.
LINES_BEFORE and LINES_AFTER control context size (default 10)."
    (claude-code-ide-mcp-server-with-session-context nil
      (let ((buf (or (find-buffer-visiting file_path)
                     (find-file-noselect file_path)))
            (before (or lines_before 10))
            (after (or lines_after 10)))
        (with-current-buffer buf
          (let* ((cur-line (line-number-at-pos (point)))
                 (start-line (max 1 (- cur-line before)))
                 (end-line (+ cur-line after))
                 (start (save-excursion
                          (goto-char (point-min))
                          (forward-line (1- start-line))
                          (point)))
                 (end (save-excursion
                        (goto-char (point-min))
                        (forward-line end-line)
                        (point))))
            (format "File: %s\nCursor at line %d, column %d\n---\n%s"
                    file_path cur-line (current-column)
                    (buffer-substring-no-properties start end)))))))

  (claude-code-ide-make-tool
   :function #'my/mcp-get-cursor-context
   :name "get_cursor_context"
   :description "Get code around the user's current cursor position in a file. Shows surrounding lines for context about what the user is looking at."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file")
           (:name "lines_before"
            :type number
            :description "Number of lines before cursor (default 10)"
            :optional t)
           (:name "lines_after"
            :type number
            :description "Number of lines after cursor (default 10)"
            :optional t)))

  ;; 4. 获取选区内容
  (defun my/mcp-get-selection (file_path)
    "Get the current active selection/region in FILE_PATH."
    (claude-code-ide-mcp-server-with-session-context nil
      (let ((buf (or (find-buffer-visiting file_path)
                     (find-file-noselect file_path))))
        (with-current-buffer buf
          (if (use-region-p)
              (let* ((beg (region-beginning))
                     (end (region-end))
                     (beg-line (line-number-at-pos beg))
                     (end-line (line-number-at-pos end)))
                (format "Selection in %s (lines %d-%d):\n%s"
                        file_path beg-line end-line
                        (buffer-substring-no-properties beg end)))
            (format "No active selection in %s" file_path))))))

  (claude-code-ide-make-tool
   :function #'my/mcp-get-selection
   :name "get_selection"
   :description "Get the currently selected/highlighted text in a file buffer. Useful for understanding what code the user has selected."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file")))

  ;; 5. 项目内 ripgrep 搜索
  (defun my/mcp-project-search (pattern &optional file_glob)
    "Search for PATTERN in project using ripgrep.
Optional FILE_GLOB limits to matching files (e.g. \"*.el\")."
    (claude-code-ide-mcp-server-with-session-context nil
      (let* ((cmd (concat "rg --no-heading --line-number --color never "
                          (when file_glob (format "-g '%s' " file_glob))
                          (format "'%s' %s"
                                  (shell-quote-argument pattern)
                                  default-directory)))
             (result (shell-command-to-string cmd)))
        (if (string-empty-p result)
            (format "No matches found for '%s'" pattern)
          result))))

  (claude-code-ide-make-tool
   :function #'my/mcp-project-search
   :name "project_search"
   :description "Search for a pattern in the current project using ripgrep. Supports file type filtering with glob patterns."
   :args '((:name "pattern"
            :type string
            :description "Search pattern (regex supported)")
           (:name "file_glob"
            :type string
            :description "Optional glob to filter files (e.g. \"*.py\", \"*.el\")"
            :optional t)))

  ;; ── @ buffer / 粘贴图片 ──────────────────────────────────

  (defun my/claude-ide-at-buffer ()
    "选择一个打开的 buffer，将其路径以 @path 形式插入 Claude 终端输入。
若当前 buffer 有选区，自动附加行号范围 @path:L1-L2。"
    (interactive)
    (let* ((buffer-name (claude-code-ide--get-buffer-name))
           (session-buf (get-buffer buffer-name)))
      (unless session-buf
        (user-error "No Claude Code session for this project"))
      ;; 收集所有 file-visiting buffer
      (let* ((candidates
              (cl-loop for buf in (buffer-list)
                       when (buffer-file-name buf)
                       collect (cons (format "%s  [%s]"
                                            (abbreviate-file-name (buffer-file-name buf))
                                            (with-current-buffer buf
                                              (symbol-name major-mode)))
                                     buf)))
             (choice (completing-read "@ buffer: " candidates nil t))
             (buf (alist-get choice candidates nil nil #'string=))
             (file-path (buffer-file-name buf))
             ;; 如果选中的 buffer 有 region，附加行号
             (ref (with-current-buffer buf
                    (if (use-region-p)
                        (let ((l1 (line-number-at-pos (region-beginning)))
                              (l2 (line-number-at-pos (region-end))))
                          (format "@%s:%d-%d" file-path l1 l2))
                      (format "@%s" file-path)))))
        ;; 插入到 Claude 终端（不回车，让用户继续输入 prompt）
        (with-current-buffer session-buf
          (claude-code-ide--terminal-send-string (concat ref " "))))))

  (defun my/claude-ide-at-current-buffer ()
    "将当前 buffer 路径以 @path 形式插入 Claude 终端输入。
若有选区，附加行号范围。"
    (interactive)
    (let* ((buffer-name (claude-code-ide--get-buffer-name))
           (session-buf (get-buffer buffer-name)))
      (unless session-buf
        (user-error "No Claude Code session for this project"))
      (let* ((file-path (or (buffer-file-name)
                            (user-error "Current buffer is not visiting a file")))
             (ref (if (use-region-p)
                      (let ((l1 (line-number-at-pos (region-beginning)))
                            (l2 (line-number-at-pos (region-end))))
                        (format "@%s:%d-%d" file-path l1 l2))
                    (format "@%s" file-path))))
        (with-current-buffer session-buf
          (claude-code-ide--terminal-send-string (concat ref " "))))))

  (defun my/claude-ide-at-project-file ()
    "从项目文件中选择，将路径以 @path 形式插入 Claude 终端输入。
使用 project-files + completing-read（vertico/orderless 自动接管）。"
    (interactive)
    (let* ((buffer-name (claude-code-ide--get-buffer-name))
           (session-buf (get-buffer buffer-name)))
      (unless session-buf
        (user-error "No Claude Code session for this project"))
      (let* ((proj (or (project-current)
                       (user-error "Not in a project")))
             (root (project-root proj))
             (files (project-files proj))
             ;; 显示相对路径，更易阅读
             (rel-files (mapcar (lambda (f) (file-relative-name f root)) files))
             (choice (completing-read "@ project file: " rel-files nil t))
             (abs-path (expand-file-name choice root)))
        (with-current-buffer session-buf
          (claude-code-ide--terminal-send-string (concat "@" abs-path " "))))))

  (defun my/claude-ide-paste-image ()
    "检测系统剪贴板是否有图片，有则保存到临时文件并以 @path 形式发送。
无图片时执行正常的终端粘贴。
依赖：brew install pngpaste"
    (interactive)
    (let* ((buffer-name (claude-code-ide--get-buffer-name))
           (session-buf (get-buffer buffer-name)))
      (unless session-buf
        (user-error "No Claude Code session for this project"))
      (let ((tmp-file (expand-file-name
                       (format "claude-paste-%s.png"
                               (format-time-string "%Y%m%d-%H%M%S"))
                       temporary-file-directory)))
        (if (and (executable-find "pngpaste")
                 (= 0 (call-process "pngpaste" nil nil nil tmp-file)))
            ;; 剪贴板有图片，已保存
            (progn
              (with-current-buffer session-buf
                (claude-code-ide--terminal-send-string (concat "@" tmp-file " ")))
              (message "Image pasted: %s" tmp-file))
          ;; 无图片，正常粘贴文本
          (with-current-buffer session-buf
            (claude-code-ide--terminal-send-string
             (or (gui-get-selection 'CLIPBOARD 'STRING)
                 (current-kill 0 t)
                 "")))))))

  ;; 绑定快捷键
  :bind (("C-c C-'" . claude-code-ide-menu)
         ("C-c @ @" . my/claude-ide-at-buffer)
         ("C-c @ ." . my/claude-ide-at-current-buffer)
         ("C-c @ f" . my/claude-ide-at-project-file)
         ("C-c @ v" . my/claude-ide-paste-image)))

(use-package gterm
  ;; 使用 cxa 的 fork（PR #4），修复 ghostty 1.3.2+ 编译问题
  ;; TODO: 主仓库合并后切回 rwc9u/emacs-libgterm
  :vc (:url "https://github.com/cxa/emacs-libgterm" :branch "main")
  :commands gterm
  :init
  (setq gterm-always-compile-module t)
  :config
  (setq gterm-shell "/bin/zsh")

  ;; 修复闪烁：用 inhibit-redisplay 抑制 erase-buffer 后的中间态显示
  (defun my/gterm-refresh-no-flicker (&rest _)
    "Override gterm--refresh: inhibit-redisplay 包裹整个渲染过程。"
    (when (bound-and-true-p gterm--term)
      (let ((inhibit-read-only t)
            (inhibit-redisplay t)
            cursor-pos)
        (erase-buffer)
        (setq cursor-pos (gterm-render gterm--term))
        (when (integerp cursor-pos)
          (goto-char cursor-pos))
        (when (fboundp 'gterm-cursor-info)
          (let* ((info (gterm-cursor-info gterm--term))
                 (visible (car info))
                 (style (cdr info)))
            (setq-local cursor-type (if visible style nil)))))))

  (advice-add 'gterm--refresh :override #'my/gterm-refresh-no-flicker)

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

(provide 'init-ml)
