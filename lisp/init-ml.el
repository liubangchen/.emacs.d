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

  ;; 通过 switch-buffer / consult-buffer 切到 claude session 时也自动滚底
  (defun my/claude-ide-auto-scroll-bottom ()
    "当窗口显示 claude session 的 vterm buffer 时，自动滚动到底部。"
    (when (and (derived-mode-p 'vterm-mode)
              (claude-code-ide--session-buffer-p (current-buffer)))
      (goto-char (point-max))
      (let ((win (get-buffer-window (current-buffer))))
        (when win
          (set-window-point win (point-max))
          (with-selected-window win
            (recenter -1))))))

  (add-hook 'window-buffer-change-functions
            (lambda (_frame)
              (my/claude-ide-auto-scroll-bottom)))

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
         ("C-c @ v" . my/claude-ide-paste-image)))

(provide 'init-ml)
