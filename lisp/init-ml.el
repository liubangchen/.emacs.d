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
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (setq claude-code-ide-cli-path
        (or (executable-find "claude")
            (executable-find "claude-internal")
            "claude"))
  (setq claude-code-ide-terminal-backend 'vterm)
  (setq claude-code-ide-enable-mcp-server t)
  (claude-code-ide-emacs-tools-setup)

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
            :optional t))))

(provide 'init-ml)
