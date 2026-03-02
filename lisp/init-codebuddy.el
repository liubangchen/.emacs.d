;;; init-codebuddy.el --- CodeBuddy chat interface for Emacs -*- lexical-binding: t; -*-

;; Author: chenlong
;; Description: 通过 codex-internal exec 与 CodeBuddy 交互的 Emacs 模块
;; 功能：弹出全局输入 buffer，绑定工作路径，输入问题后调用 codex-internal exec 执行
;;       思考过程在临时 buffer 显示，最终结果在输入 buffer 显示

;;; Code:

(require 'markdown-mode)
(require 'nerd-icons)

;; ============================================================
;; 自定义变量
;; ============================================================

(defgroup codebuddy nil
  "CodeBuddy chat interface."
  :group 'tools
  :prefix "codebuddy-")

(defcustom codebuddy-executable "codex-internal"
  "codex-internal 可执行文件路径."
  :type 'string
  :group 'codebuddy)

(defcustom codebuddy-default-args '("exec" "--full-auto")
  "codex-internal exec 的默认参数列表."
  :type '(repeat string)
  :group 'codebuddy)

(defcustom codebuddy-thinking-buffer-name "*CodeBuddy-Thinking*"
  "思考过程临时 buffer 名称."
  :type 'string
  :group 'codebuddy)

(defcustom codebuddy-chat-buffer-name "*CodeBuddy-Chat*"
  "全局输入/输出 buffer 名称."
  :type 'string
  :group 'codebuddy)

(defcustom codebuddy-max-context-chars 80000
  "上下文拼接的最大字符数 (约 20K tokens).
超出时从最早的对话轮次开始裁剪."
  :type 'integer
  :group 'codebuddy)

(defcustom codebuddy-db-dir-name ".codebuddy"
  "工作目录下存放 SQLite 数据库的子目录名."
  :type 'string
  :group 'codebuddy)

;; ============================================================
;; 内部变量
;; ============================================================

(defvar codebuddy--work-directory nil
  "当前 chat buffer 绑定的工作路径.")

(defvar codebuddy--process nil
  "当前运行的 codex-internal 进程.")

(defvar codebuddy--output-buffer ""
  "累积的进程输出（用于解析思考/结果）.")

(defvar codebuddy--line-buffer ""
  "跨批次的不完整行缓冲.
进程输出可能在行中间截断，此变量保存未以换行结尾的残余数据.")

(defvar codebuddy--is-thinking nil
  "当前是否处于思考阶段.")

(defvar codebuddy--output-phase 'header
  "当前输出解析阶段.
可选值: header / user / thinking / exec / codex / tokens / result.")

(defvar codebuddy--history nil
  "输入历史记录.")

(defvar codebuddy--history-index -1
  "当前历史记录索引.")

(defvar codebuddy--db nil
  "当前工作目录的 SQLite 数据库连接.")

(defvar codebuddy--current-session-id nil
  "当前活跃会话的 ID.")

(defvar codebuddy--current-response ""
  "累积当前 codex 阶段的 AI 回复文本.")

(defvar codebuddy--last-tokens-used nil
  "上次 codex-internal 报告的实际 token 使用数.")

;; ============================================================
;; Nerd Icons
;; ============================================================

(defvar codebuddy--icon-robot
  (nerd-icons-mdicon "nf-md-robot" :face '(:foreground "#61afef"))
  "Robot 图标 (标题/AI 回复).")

(defvar codebuddy--icon-user
  (nerd-icons-mdicon "nf-md-account_circle_outline" :face '(:foreground "#98c379"))
  "User 图标 (用户输入 prompt).")

(defvar codebuddy--icon-folder
  (nerd-icons-mdicon "nf-md-folder_outline" :face '(:foreground "#e5c07b"))
  "文件夹图标 (工作目录).")

(defvar codebuddy--icon-send
  (nerd-icons-mdicon "nf-md-send" :face '(:foreground "#61afef"))
  "发送图标 (RET).")

(defvar codebuddy--icon-stop
  (nerd-icons-mdicon "nf-md-stop_circle_outline" :face '(:foreground "#e06c75"))
  "停止图标 (C-c C-c).")

(defvar codebuddy--icon-close
  (nerd-icons-mdicon "nf-md-close_circle_outline" :face '(:foreground "#e06c75"))
  "关闭图标 (C-c C-k).")

(defvar codebuddy--icon-directory
  (nerd-icons-mdicon "nf-md-swap_horizontal" :face '(:foreground "#e5c07b"))
  "切换目录图标 (C-c C-d).")

(defvar codebuddy--icon-check
  (nerd-icons-mdicon "nf-md-check_circle_outline" :face '(:foreground "#98c379"))
  "完成图标.")

(defvar codebuddy--icon-error
  (nerd-icons-mdicon "nf-md-close_circle_outline" :face '(:foreground "#e06c75"))
  "错误/中断图标.")

(defvar codebuddy--icon-warning
  (nerd-icons-mdicon "nf-md-alert_circle_outline" :face '(:foreground "#e5c07b"))
  "警告图标.")

(defvar codebuddy--icon-loading
  (nerd-icons-mdicon "nf-md-loading" :face '(:foreground "#c678dd"))
  "加载中图标.")

(defvar codebuddy--icon-thinking
  (nerd-icons-mdicon "nf-md-brain" :face '(:foreground "#5c6370"))
  "思考图标.")

(defvar codebuddy--icon-keyboard
  (nerd-icons-mdicon "nf-md-keyboard" :face '(:foreground "#abb2bf"))
  "键盘/快捷键图标.")

;; ============================================================
;; Faces
;; ============================================================

(defface codebuddy-prompt-face
  '((t :foreground "#61afef" :weight bold))
  "Prompt 文字样式."
  :group 'codebuddy)

(defface codebuddy-thinking-face
  '((t :foreground "#5c6370" :slant italic))
  "思考过程文字样式."
  :group 'codebuddy)

(defface codebuddy-status-face
  '((t :foreground "#c678dd" :weight bold))
  "状态信息样式."
  :group 'codebuddy)

;; ============================================================
;; SQLite 数据库层
;; ============================================================

(defun codebuddy--db-path ()
  "返回当前工作目录的数据库文件路径."
  (let ((dir (expand-file-name codebuddy-db-dir-name
                               codebuddy--work-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (expand-file-name "chat.db" dir)))

(defun codebuddy--db-init ()
  "初始化 SQLite 数据库连接并创建表结构."
  (when codebuddy--db
    (sqlite-close codebuddy--db))
  (setq codebuddy--db (sqlite-open (codebuddy--db-path)))
  (sqlite-execute codebuddy--db
                  "CREATE TABLE IF NOT EXISTS sessions (
                     id         INTEGER PRIMARY KEY AUTOINCREMENT,
                     created_at TEXT    DEFAULT (datetime('now','localtime')),
                     title      TEXT,
                     is_active  INTEGER DEFAULT 1
                   )")
  (sqlite-execute codebuddy--db
                  "CREATE TABLE IF NOT EXISTS messages (
                     id          INTEGER PRIMARY KEY AUTOINCREMENT,
                     session_id  INTEGER NOT NULL REFERENCES sessions(id),
                     role        TEXT    NOT NULL,
                     content     TEXT    NOT NULL,
                     tokens_est  INTEGER,
                     tokens_used INTEGER,
                     created_at  TEXT    DEFAULT (datetime('now','localtime'))
                   )")
  (sqlite-execute codebuddy--db
                  "CREATE INDEX IF NOT EXISTS idx_messages_session
                   ON messages(session_id, created_at)"))

(defun codebuddy--db-ensure-session ()
  "确保有一个活跃会话，没有则创建."
  (let ((rows (sqlite-select codebuddy--db
                             "SELECT id FROM sessions WHERE is_active = 1
                              ORDER BY id DESC LIMIT 1")))
    (if rows
        (setq codebuddy--current-session-id (caar rows))
      (codebuddy--db-new-session))))

(defun codebuddy--db-new-session ()
  "创建新会话并设为活跃."
  ;; 将所有旧会话标记为非活跃
  (sqlite-execute codebuddy--db
                  "UPDATE sessions SET is_active = 0 WHERE is_active = 1")
  (sqlite-execute codebuddy--db
                  "INSERT INTO sessions (title) VALUES (NULL)")
  (setq codebuddy--current-session-id
        (caar (sqlite-select codebuddy--db "SELECT last_insert_rowid()"))))

(defun codebuddy--db-save-message (role content &optional tokens-used)
  "保存一条消息到当前会话.
ROLE 为 \"user\" 或 \"assistant\", CONTENT 为消息文本.
可选 TOKENS-USED 为实际消耗 token 数."
  (let ((tokens-est (/ (length content) 4)))
    (sqlite-execute codebuddy--db
                    "INSERT INTO messages (session_id, role, content, tokens_est, tokens_used)
                     VALUES (?, ?, ?, ?, ?)"
                    (list codebuddy--current-session-id role content
                          tokens-est tokens-used)))
  ;; 如果是第一条 user 消息，自动设置会话标题
  (when (string= role "user")
    (let ((existing (caar (sqlite-select
                           codebuddy--db
                           "SELECT title FROM sessions WHERE id = ?"
                           (list codebuddy--current-session-id)))))
      (when (or (null existing) (string-empty-p existing))
        (sqlite-execute codebuddy--db
                        "UPDATE sessions SET title = ? WHERE id = ?"
                        (list (truncate-string-to-width content 40 nil nil "...")
                              codebuddy--current-session-id))))))

(defun codebuddy--db-get-session-messages (&optional session-id)
  "获取指定会话的所有消息，返回 ((role content) ...) 列表.
默认使用当前活跃会话."
  (let ((sid (or session-id codebuddy--current-session-id)))
    (sqlite-select codebuddy--db
                   "SELECT role, content FROM messages
                    WHERE session_id = ? ORDER BY created_at ASC"
                   (list sid))))

(defun codebuddy--db-get-session-stats (&optional session-id)
  "获取会话统计信息，返回 (轮数 总字符数 总估算token 总实际token).
默认使用当前活跃会话."
  (let ((sid (or session-id codebuddy--current-session-id)))
    (car (sqlite-select
          codebuddy--db
          "SELECT COUNT(*)/2, SUM(LENGTH(content)),
                  SUM(tokens_est), SUM(COALESCE(tokens_used, 0))
           FROM messages WHERE session_id = ?"
          (list sid)))))

(defun codebuddy--db-list-sessions ()
  "列出当前目录的所有会话, 返回 ((id title created_at is_active msg_count) ...)."
  (sqlite-select
   codebuddy--db
   "SELECT s.id, s.title, s.created_at, s.is_active,
           (SELECT COUNT(*) FROM messages m WHERE m.session_id = s.id)
    FROM sessions s ORDER BY s.id DESC"))

(defun codebuddy--db-switch-session (target-id)
  "切换到指定会话 TARGET-ID."
  (sqlite-execute codebuddy--db
                  "UPDATE sessions SET is_active = 0 WHERE is_active = 1")
  (sqlite-execute codebuddy--db
                  "UPDATE sessions SET is_active = 1 WHERE id = ?"
                  (list target-id))
  (setq codebuddy--current-session-id target-id))

(defun codebuddy--db-delete-session (session-id)
  "删除指定会话 SESSION-ID 及其所有消息.
返回被删除的消息数."
  (let ((msg-count (caar (sqlite-select
                          codebuddy--db
                          "SELECT COUNT(*) FROM messages WHERE session_id = ?"
                          (list session-id)))))
    (sqlite-execute codebuddy--db
                    "DELETE FROM messages WHERE session_id = ?"
                    (list session-id))
    (sqlite-execute codebuddy--db
                    "DELETE FROM sessions WHERE id = ?"
                    (list session-id))
    ;; 如果删除的是当前活跃会话，自动创建新会话
    (when (eql session-id codebuddy--current-session-id)
      (codebuddy--db-new-session))
    (or msg-count 0)))

(defun codebuddy--db-delete-all-sessions ()
  "删除所有会话和消息，返回被删除的会话数."
  (let ((count (caar (sqlite-select
                      codebuddy--db
                      "SELECT COUNT(*) FROM sessions"))))
    (sqlite-execute codebuddy--db "DELETE FROM messages")
    (sqlite-execute codebuddy--db "DELETE FROM sessions")
    ;; 创建新会话
    (codebuddy--db-new-session)
    (or count 0)))

(defun codebuddy--db-purge-inactive ()
  "删除所有非活跃会话，只保留当前会话.
返回被删除的会话数."
  (let ((count (caar (sqlite-select
                      codebuddy--db
                      "SELECT COUNT(*) FROM sessions WHERE is_active = 0"))))
    (sqlite-execute codebuddy--db
                    "DELETE FROM messages WHERE session_id IN
                     (SELECT id FROM sessions WHERE is_active = 0)")
    (sqlite-execute codebuddy--db
                    "DELETE FROM sessions WHERE is_active = 0")
    (or count 0)))

(defun codebuddy--db-close ()
  "关闭数据库连接."
  (when codebuddy--db
    (sqlite-close codebuddy--db)
    (setq codebuddy--db nil)))

;; ============================================================
;; 上下文拼接
;; ============================================================

(defun codebuddy--build-context-prompt (new-input)
  "基于当前会话历史和 NEW-INPUT 构建完整的上下文 prompt.
会根据 `codebuddy-max-context-chars' 裁剪早期对话."
  (let* ((messages (codebuddy--db-get-session-messages))
         (pairs '())
         (current-pair nil)
         (skipped 0))
    ;; 将消息组装为 (user . assistant) 对
    (dolist (msg messages)
      (let ((role (car msg))
            (content (cadr msg)))
        (cond
         ((string= role "user")
          (when current-pair
            (push current-pair pairs))
          (setq current-pair (cons content nil)))
         ((string= role "assistant")
          (when current-pair
            (setcdr current-pair content)
            (push current-pair pairs)
            (setq current-pair nil))))))
    (when current-pair
      (push current-pair pairs))
    (setq pairs (nreverse pairs))
    ;; 裁剪：从最早的对话开始删除，直到总字符数在预算内
    (let ((total-chars (+ (length new-input)
                          (apply #'+ (mapcar (lambda (p)
                                               (+ (length (car p))
                                                  (length (or (cdr p) ""))))
                                             pairs)))))
      (while (and pairs (> total-chars codebuddy-max-context-chars))
        (let ((removed (pop pairs)))
          (setq total-chars (- total-chars
                               (length (car removed))
                               (length (or (cdr removed) ""))))
          (setq skipped (1+ skipped)))))
    ;; 拼接 prompt
    (let ((parts '()))
      (when (> (length pairs) 0)
        (push "[以下是之前的对话上下文，请基于此继续回答]\n" parts)
        (when (> skipped 0)
          (push (format "[已省略更早的 %d 轮对话]\n\n" skipped) parts))
        (let ((idx 1))
          (dolist (pair pairs)
            (push (format "### 对话 %d\n**User**: %s\n" idx (car pair)) parts)
            (when (cdr pair)
              (push (format "**Assistant**: %s\n" (cdr pair)) parts))
            (push "\n" parts)
            (setq idx (1+ idx))))
        (push "---\n[当前问题]\n" parts))
      (push new-input parts)
      (apply #'concat (nreverse parts)))))

;; ============================================================
;; 魔法指令系统
;; ============================================================

(defvar codebuddy--icon-magic
  (nerd-icons-mdicon "nf-md-slash_forward_box" :face '(:foreground "#c678dd"))
  "魔法指令图标.")

(defvar codebuddy--icon-info
  (nerd-icons-mdicon "nf-md-information_outline" :face '(:foreground "#61afef"))
  "信息图标.")

(defvar codebuddy--icon-chart
  (nerd-icons-mdicon "nf-md-chart_bar" :face '(:foreground "#98c379"))
  "统计图标.")

(defvar codebuddy--icon-history
  (nerd-icons-mdicon "nf-md-history" :face '(:foreground "#e5c07b"))
  "历史图标.")

(defvar codebuddy--icon-new
  (nerd-icons-mdicon "nf-md-plus_circle_outline" :face '(:foreground "#98c379"))
  "新建图标.")

(defvar codebuddy--icon-delete
  (nerd-icons-mdicon "nf-md-delete_outline" :face '(:foreground "#e06c75"))
  "删除图标.")

(defvar codebuddy--icon-broom
  (nerd-icons-mdicon "nf-md-broom" :face '(:foreground "#e5c07b"))
  "清理图标.")

(defvar codebuddy--slash-commands
  '("/new" "/clear" "/tokens" "/history" "/session " "/delete " "/delete all" "/purge" "/help")
  "所有可用的魔法指令列表.
带尾部空格的表示需要参数（如 /session N, /delete N）.")

(defun codebuddy--handle-slash-command (input)
  "处理以 / 开头的魔法指令.
返回 t 表示已处理（不需要发送到 codex），nil 表示非指令."
  (let ((cmd (string-trim input)))
    (cond
     ;; /new - 新建会话
     ((string= cmd "/new")
      (codebuddy--cmd-new)
      t)
     ;; /clear - 清屏 + 新建会话
     ((string= cmd "/clear")
      (codebuddy--cmd-clear)
      t)
     ;; /tokens - 显示 token 使用信息
     ((string= cmd "/tokens")
      (codebuddy--cmd-tokens)
      t)
     ;; /history - 列出所有历史会话
     ((string= cmd "/history")
      (codebuddy--cmd-history)
      t)
     ;; /session N - 切换到第 N 个会话
     ((string-match "^/session\\s-+\\([0-9]+\\)$" cmd)
      (codebuddy--cmd-session (string-to-number (match-string 1 cmd)))
      t)
     ;; /delete all - 删除所有会话
     ((string= cmd "/delete all")
      (codebuddy--cmd-delete-all)
      t)
     ;; /delete N - 删除指定会话
     ((string-match "^/delete\\s-+\\([0-9]+\\)$" cmd)
      (codebuddy--cmd-delete (string-to-number (match-string 1 cmd)))
      t)
     ;; /purge - 清理非活跃的旧会话
     ((string= cmd "/purge")
      (codebuddy--cmd-purge)
      t)
     ;; /help - 显示帮助
     ((string= cmd "/help")
      (codebuddy--cmd-help)
      t)
     ;; 不是指令
     (t nil))))

(defun codebuddy--cmd-new ()
  "执行 /new: 新建会话."
  (codebuddy--db-new-session)
  (codebuddy--append-to-chat
   (format "\n\n---\n\n## %s 新会话\n\n已开启新的会话 (ID: %d)，上下文已重置。\n"
           codebuddy--icon-new
           codebuddy--current-session-id))
  (codebuddy--insert-prompt))

(defun codebuddy--cmd-clear ()
  "执行 /clear: 清屏 + 新建会话."
  (codebuddy--db-new-session)
  (when-let* ((buf (get-buffer codebuddy-chat-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (codebuddy--insert-header))))
  (codebuddy--clear-thinking-buffer)
  (codebuddy--append-to-chat
   (format "\n%s 已清屏并开启新会话 (ID: %d)\n"
           codebuddy--icon-new
           codebuddy--current-session-id))
  (codebuddy--insert-prompt))

(defun codebuddy--cmd-tokens ()
  "执行 /tokens: 显示 token 使用信息."
  (let* ((stats (codebuddy--db-get-session-stats))
         (rounds (or (nth 0 stats) 0))
         (chars (or (nth 1 stats) 0))
         (est-tokens (or (nth 2 stats) 0))
         (used-tokens (or (nth 3 stats) 0))
         (budget-pct (if (> codebuddy-max-context-chars 0)
                         (min 100 (/ (* chars 100) codebuddy-max-context-chars))
                       0)))
    (codebuddy--append-to-chat
     (format "\n\n---\n\n%s **Token 使用统计**\n\n| 项目 | 数值 |\n|---|---|\n| 当前会话 | #%d |\n| 对话轮数 | %d 轮 |\n| 上下文字符数 | %s |\n| 估算 Token 数 | ~%s |\n| 实际消耗 Token | %s |\n| 上下文预算 | %d%% (%s / %s chars) |\n\n"
             codebuddy--icon-chart
             codebuddy--current-session-id
             rounds
             (codebuddy--format-number chars)
             (codebuddy--format-number est-tokens)
             (if (> used-tokens 0)
                 (codebuddy--format-number used-tokens)
               "N/A")
             budget-pct
             (codebuddy--format-number chars)
             (codebuddy--format-number codebuddy-max-context-chars))))
  (codebuddy--align-tables)
  (codebuddy--insert-prompt))

(defun codebuddy--cmd-history ()
  "执行 /history: 列出所有历史会话."
  (let ((sessions (codebuddy--db-list-sessions)))
    (if (null sessions)
        (codebuddy--append-to-chat
         (format "\n\n%s 暂无历史会话记录。\n" codebuddy--icon-info))
      (codebuddy--append-to-chat
       (format "\n\n---\n\n%s **历史会话列表**\n\n| ID | 标题 | 创建时间 | 状态 | 消息数 |\n|---|---|---|---|---|\n"
               codebuddy--icon-history))
      (dolist (s sessions)
        (let ((id (nth 0 s))
              (title (or (nth 1 s) "(未命名)"))
              (created (nth 2 s))
              (active (nth 3 s))
              (msg-count (nth 4 s)))
          (codebuddy--append-to-chat
           (format "| %d | %s | %s | %s | %d |\n"
                   id title created
                   (if (= active 1) "**当前**" "-")
                   msg-count))))
      (codebuddy--append-to-chat
       "\n> 使用 `/session N` 切换到指定会话\n\n")))
  (codebuddy--align-tables)
  (codebuddy--insert-prompt))

(defun codebuddy--cmd-session (target-id)
  "执行 /session N: 切换到会话 TARGET-ID."
  (let ((exists (caar (sqlite-select
                       codebuddy--db
                       "SELECT COUNT(*) FROM sessions WHERE id = ?"
                       (list target-id)))))
    (if (and exists (> exists 0))
        (progn
          (codebuddy--db-switch-session target-id)
          (let ((stats (codebuddy--db-get-session-stats target-id)))
            (codebuddy--append-to-chat
             (format "\n\n---\n\n%s 已切换到会话 #%d (%d 轮对话，~%s tokens 上下文)\n"
                     codebuddy--icon-history
                     target-id
                     (or (nth 0 stats) 0)
                     (codebuddy--format-number (or (nth 2 stats) 0))))))
      (codebuddy--append-to-chat
       (format "\n\n%s 会话 #%d 不存在，请用 `/history` 查看可用会话。\n"
               codebuddy--icon-warning target-id))))
  (codebuddy--insert-prompt))

(defun codebuddy--cmd-delete (target-id)
  "执行 /delete N: 删除指定会话 TARGET-ID."
  (let ((exists (caar (sqlite-select
                       codebuddy--db
                       "SELECT COUNT(*) FROM sessions WHERE id = ?"
                       (list target-id)))))
    (if (and exists (> exists 0))
        (let ((msg-count (codebuddy--db-delete-session target-id)))
          (codebuddy--append-to-chat
           (format "\n\n---\n\n%s 已删除会话 #%d（%d 条消息）\n"
                   codebuddy--icon-delete target-id msg-count))
          (when (eql target-id codebuddy--current-session-id)
            ;; 当前会话被删除后，db-delete-session 已自动创建新会话
            ;; 这里 current-session-id 已经更新了，因为 eql 比较的是删前的值
            nil)
          (codebuddy--append-to-chat
           (format "> 当前活跃会话: #%d\n" codebuddy--current-session-id)))
      (codebuddy--append-to-chat
       (format "\n\n%s 会话 #%d 不存在，请用 `/history` 查看可用会话。\n"
               codebuddy--icon-warning target-id))))
  (codebuddy--insert-prompt))

(defun codebuddy--cmd-delete-all ()
  "执行 /delete all: 删除所有会话."
  (let ((count (codebuddy--db-delete-all-sessions)))
    (codebuddy--append-to-chat
     (format "\n\n---\n\n%s 已删除全部 %d 个会话，已自动开启新会话 #%d\n"
             codebuddy--icon-delete count codebuddy--current-session-id)))
  (codebuddy--insert-prompt))

(defun codebuddy--cmd-purge ()
  "执行 /purge: 删除所有非活跃的旧会话，只保留当前会话."
  (let ((count (codebuddy--db-purge-inactive)))
    (if (> count 0)
        (codebuddy--append-to-chat
         (format "\n\n---\n\n%s 已清理 %d 个旧会话，仅保留当前会话 #%d\n"
                 codebuddy--icon-broom count codebuddy--current-session-id))
      (codebuddy--append-to-chat
       (format "\n\n%s 没有需要清理的旧会话。\n" codebuddy--icon-info))))
  (codebuddy--insert-prompt))

(defun codebuddy--cmd-help ()
  "执行 /help: 显示所有可用指令."
  (codebuddy--append-to-chat
   (format "\n\n---\n\n%s **可用指令**\n\n| 指令 | 功能 |\n|---|---|\n| `/new` | 新建会话（保留 buffer 内容） |\n| `/clear` | 清屏 + 新建会话 |\n| `/tokens` | 显示当前上下文 Token 使用统计 |\n| `/history` | 列出所有历史会话 |\n| `/session N` | 切换到第 N 个历史会话 |\n| `/delete N` | 删除指定会话 #N 及其所有消息 |\n| `/delete all` | 删除所有会话（清空历史） |\n| `/purge` | 清理旧会话，只保留当前会话 |\n| `/help` | 显示此帮助 |\n\n"
           codebuddy--icon-magic))
  (codebuddy--align-tables)
  (codebuddy--insert-prompt))

(defun codebuddy--format-number (n)
  "将数字 N 格式化为带千分位的字符串."
  (let* ((s (number-to-string (truncate n)))
         (len (length s))
         (parts '())
         (i 0))
    (while (< i len)
      (let* ((end (- len i))
             (start (max 0 (- end 3))))
        (push (substring s start end) parts)
        (setq i (+ i (- end start)))))
    (mapconcat #'identity parts ",")))

;; ============================================================
;; Token 解析
;; ============================================================

(defun codebuddy--parse-tokens-line (line)
  "尝试从 tokens 阶段的行中解析 token 使用数.
格式通常为 \\='input: NNN  output: NNN  total: NNN\\=' 或类似."
  (when (string-match "total[: ]+\\([0-9,]+\\)" line)
    (let ((num-str (replace-regexp-in-string "," "" (match-string 1 line))))
      (setq codebuddy--last-tokens-used
            (string-to-number num-str)))))

(defun codebuddy--insert-token-stats ()
  "在 chat buffer 中插入本次交互的 token 统计摘要."
  (let* ((stats (codebuddy--db-get-session-stats))
         (rounds (or (nth 0 stats) 0))
         (chars (or (nth 1 stats) 0))
         (est-tokens (or (nth 2 stats) 0)))
    (codebuddy--append-to-chat
     (format "\n> %s 上下文: ~%s tokens (%s chars, %d轮)%s\n"
             codebuddy--icon-chart
             (codebuddy--format-number est-tokens)
             (codebuddy--format-number chars)
             rounds
             (if codebuddy--last-tokens-used
                 (format " | 本次消耗: %s tokens"
                         (codebuddy--format-number codebuddy--last-tokens-used))
               "")))))

;; ============================================================
;; Keymap
;; ============================================================

(defvar codebuddy-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'codebuddy-send-input)
    (define-key map (kbd "TAB") #'codebuddy-complete-slash-command)
    (define-key map (kbd "C-c C-c") #'codebuddy-interrupt)
    (define-key map (kbd "C-c C-k") #'codebuddy-kill-chat)
    (define-key map (kbd "C-c C-l") #'codebuddy-clear-chat)
    (define-key map (kbd "C-c C-d") #'codebuddy-change-directory)
    (define-key map (kbd "M-p") #'codebuddy-history-prev)
    (define-key map (kbd "M-n") #'codebuddy-history-next)
    map)
  "CodeBuddy chat mode keymap.")

;; ============================================================
;; Major Mode
;; ============================================================

(define-derived-mode codebuddy-chat-mode markdown-mode "CodeBuddy"
  "CodeBuddy Chat 交互模式.
基于 markdown-mode，自动渲染 Markdown 输出.

\\{codebuddy-chat-mode-map}"
  (setq-local buffer-read-only nil)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (visual-line-mode 1)
  ;; 隐藏 Markdown 标记符号（**、#、` 等），接近所见即所得
  (when (fboundp 'markdown-toggle-markup-hiding)
    (markdown-toggle-markup-hiding 1)))

;; ============================================================
;; 核心功能
;; ============================================================

(defvar codebuddy--prompt-string "You> "
  "Prompt 字符串，用于定位用户输入区域.")

;; ============================================================
;; 指令补全
;; ============================================================

(defun codebuddy--input-start-pos ()
  "返回当前 prompt 输入区域的起始位置，找不到则返回 nil."
  (save-excursion
    (goto-char (point-max))
    (when (search-backward codebuddy--prompt-string nil t)
      (+ (point) (length codebuddy--prompt-string)))))

(defun codebuddy-complete-slash-command ()
  "对以 / 开头的输入进行魔法指令补全.
如果不在输入区域或输入不以 / 开头，执行默认 Tab 行为."
  (interactive)
  (let ((input-start (codebuddy--input-start-pos)))
    (if (and input-start (>= (point) input-start))
        (let* ((input (buffer-substring-no-properties input-start (point-max)))
               (trimmed (string-trim input)))
          (if (string-prefix-p "/" trimmed)
              ;; 以 / 开头：进行补全
              (let* ((candidates (cl-remove-if-not
                                  (lambda (cmd)
                                    (string-prefix-p trimmed cmd))
                                  codebuddy--slash-commands))
                     (common (try-completion trimmed
                                             (mapcar #'list candidates))))
                (cond
                 ;; 唯一匹配或完整匹配
                 ((eq common t)
                  (message "已是完整指令"))
                 ;; 有公共前缀可以扩展
                 ((and (stringp common) (not (string= common trimmed)))
                  (let ((inhibit-read-only t))
                    (delete-region input-start (point-max))
                    (goto-char input-start)
                    (insert common))
                  ;; 如果扩展后唯一匹配，提示
                  (when (= (length candidates) 1)
                    (message "%s" (car candidates))))
                 ;; 多个候选，在 minibuffer 展示选择
                 ((and candidates (> (length candidates) 1))
                  (let ((chosen (completing-read "指令: " candidates nil t trimmed)))
                    (when chosen
                      (let ((inhibit-read-only t))
                        (delete-region input-start (point-max))
                        (goto-char input-start)
                        (insert chosen)))))
                 ;; 无匹配
                 (t (message "无匹配的指令"))))
            ;; 不以 / 开头：默认 Tab 行为
            (indent-for-tab-command)))
      ;; 不在输入区域
      (indent-for-tab-command))))

(defun codebuddy--insert-prompt ()
  "在 chat buffer 末尾插入输入提示符."
  (with-current-buffer (get-buffer codebuddy-chat-buffer-name)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert "\n---\n")
      (insert (format "%s `[%s]`\n"
                      codebuddy--icon-folder
                      (abbreviate-file-name codebuddy--work-directory)))
      (insert (propertize (format "%s You> " codebuddy--icon-user)
                          'face 'codebuddy-prompt-face)))
    (goto-char (point-max))))

(defun codebuddy--insert-header ()
  "在 chat buffer 插入欢迎头部信息 (Markdown 格式 + nerd-icons)."
  (let ((inhibit-read-only t))
    (insert (format "# %s CodeBuddy Chat\n\n---\n\n"
                    codebuddy--icon-robot))
    (insert (format "> %s 工作目录: `%s`\n"
                    codebuddy--icon-folder
                    codebuddy--work-directory))
    (insert (format "> %s 快捷键: %s `RET` 发送 | %s `C-c C-c` 中断 | %s `C-c C-k` 关闭 | %s `C-c C-d` 切换目录\n"
                    codebuddy--icon-keyboard
                    codebuddy--icon-send
                    codebuddy--icon-stop
                    codebuddy--icon-close
                    codebuddy--icon-directory))
    (insert (format "> %s 输入 `/help` 查看所有指令\n\n---\n"
                    codebuddy--icon-magic))))

(defun codebuddy--get-user-input ()
  "获取当前 prompt 后的用户输入文本."
  (with-current-buffer (get-buffer codebuddy-chat-buffer-name)
    (save-excursion
      (goto-char (point-max))
      (let ((end (point)))
        ;; 向前搜索最后一个 "You> " 文本
        (if (search-backward codebuddy--prompt-string nil t)
            (progn
              (forward-char (length codebuddy--prompt-string))
              (string-trim (buffer-substring-no-properties (point) end)))
          "")))))

(defun codebuddy--append-to-chat (text)
  "在 chat buffer 中追加 TEXT，并刷新 markdown font-lock."
  (when-let* ((buf (get-buffer codebuddy-chat-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (start (point-max)))
        (goto-char start)
        (insert text)
        ;; 刷新新插入区域的 font-lock
        (font-lock-flush start (point-max))))))

(defun codebuddy--align-tables ()
  "对齐 chat buffer 中所有 Markdown 表格."
  (when-let* ((buf (get-buffer codebuddy-chat-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*|" nil t)
            (beginning-of-line)
            (when (markdown-table-at-point-p)
              (condition-case nil
                  (markdown-table-align)
                (error nil)))
            (forward-line 1)))))))

(defun codebuddy--append-to-thinking (text)
  "在 thinking buffer 中追加 TEXT."
  (let ((buf (get-buffer-create codebuddy-thinking-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'special-mode)
        (special-mode)
        (setq-local buffer-read-only nil)
        (setq-local truncate-lines nil)
        (setq-local word-wrap t))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize text 'face 'codebuddy-thinking-face))))
    ;; 自动滚动 thinking buffer（如果可见）
    (when-let* ((win (get-buffer-window buf t)))
      (with-selected-window win
        (goto-char (point-max))
        (recenter -3)))))

(defun codebuddy--clear-thinking-buffer ()
  "清除 thinking buffer 内容并写入新的 header."
  (let ((buf (get-buffer-create codebuddy-thinking-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "%s CodeBuddy Thinking\n\n"
                                    codebuddy--icon-thinking)
                            'face 'codebuddy-status-face))))))

(defun codebuddy--dispatch-line (line)
  "根据当前状态机阶段分发单行 LINE 到对应 buffer."
  (let ((trimmed (string-trim line)))
    (cond
     ;; ---- 阶段切换检测 ----

     ;; 分隔线 --------: header 阶段标记
     ((string-match-p "^--------$" trimmed)
      (setq codebuddy--output-phase 'header)
      (codebuddy--append-to-thinking (concat line "\n")))

     ;; "user" 独占一行 -> 进入 user 阶段
     ((and (string= "user" trimmed)
           (memq codebuddy--output-phase '(header user)))
      (setq codebuddy--output-phase 'user)
      (codebuddy--append-to-thinking (concat line "\n")))

     ;; "thinking" 独占一行 -> 进入 thinking 阶段
     ((string= "thinking" trimmed)
      (setq codebuddy--output-phase 'thinking)
      (codebuddy--append-to-thinking (concat line "\n")))

     ;; "exec" 开头 -> 进入 exec 阶段 (工具调用)
     ((string-match-p "^exec$" trimmed)
      (setq codebuddy--output-phase 'exec)
      (codebuddy--append-to-thinking (concat line "\n")))

     ;; "mcp" 开头 -> mcp 信息，归入 thinking
     ((string-match-p "^mcp " trimmed)
      (codebuddy--append-to-thinking (concat line "\n")))

     ;; "codex" 独占一行 -> 进入 codex 阶段（结果输出到 chat buffer）
     ((string= "codex" trimmed)
      (setq codebuddy--output-phase 'codex))

     ;; "tokens used" -> 进入 tokens 阶段，之后所有内容忽略/归入 thinking
     ((string-match-p "^tokens used$" trimmed)
      (setq codebuddy--output-phase 'tokens)
      (codebuddy--append-to-thinking (concat "\n" line "\n")))

     ;; ---- 各阶段内容分发 ----

     ;; tokens 阶段: 统计信息及之后的重复结果 -> thinking（忽略）
     ;; 同时尝试解析 token 使用数
     ((eq codebuddy--output-phase 'tokens)
      (codebuddy--parse-tokens-line line)
      (codebuddy--append-to-thinking (concat line "\n")))

     ;; codex 阶段: 最终结果 -> chat buffer (原始 Markdown，由 markdown-mode 渲染)
     ;; 同时累积回复文本，用于存入 DB
     ((eq codebuddy--output-phase 'codex)
      (setq codebuddy--current-response
            (concat codebuddy--current-response line "\n"))
      (codebuddy--append-to-chat (concat line "\n")))

     ;; header/user/thinking/exec 阶段: 全部 -> thinking buffer
     ((memq codebuddy--output-phase '(header user thinking exec))
      (codebuddy--append-to-thinking (concat line "\n")))

     ;; 未知阶段，默认输出到 thinking
     (t
      (codebuddy--append-to-thinking (concat line "\n"))))))

(defun codebuddy--process-filter (_proc output)
  "处理 codex-internal 的输出 OUTPUT.
正确处理跨批次的不完整行：进程输出可能在任意字节处截断，
将上次残余的 line-buffer 与本次 output 拼接后按换行切分，
最后一段若无换行则暂存到 line-buffer 留待下次拼接."
  ;; 拼接上次残余
  (let* ((data (concat codebuddy--line-buffer output))
         (has-trailing-newline (string-suffix-p "\n" data))
         (lines (split-string data "\n")))
    ;; 如果末尾不是换行，最后一个元素是不完整行，暂存
    (if has-trailing-newline
        (progn
          (setq codebuddy--line-buffer "")
          ;; split-string 在末尾换行时会多出一个空字符串，去掉
          (when (and lines (string-empty-p (car (last lines))))
            (setq lines (butlast lines))))
      ;; 末尾无换行：最后一段是不完整行
      (setq codebuddy--line-buffer (car (last lines)))
      (setq lines (butlast lines)))
    ;; 逐完整行分发
    (dolist (line lines)
      (codebuddy--dispatch-line line)))
  ;; 自动滚动 chat buffer
  (when-let* ((win (get-buffer-window codebuddy-chat-buffer-name t)))
    (with-selected-window win
      (goto-char (point-max))
      (recenter -3))))

(defun codebuddy--process-sentinel (_proc event)
  "codex-internal 进程结束时的处理函数."
  (let ((event-str (string-trim event)))
    (cond
     ((string-match-p "finished" event-str)
      (codebuddy--append-to-chat
       (format "\n\n---\n\n**%s 执行完成**\n" codebuddy--icon-check)))
     ((string-match-p "\\(killed\\|interrupt\\)" event-str)
      (codebuddy--append-to-chat
       (format "\n\n---\n\n**%s 已中断**\n" codebuddy--icon-error)))
     (t
      (codebuddy--append-to-chat
       (format "\n\n---\n\n**%s 进程退出: %s**\n"
               codebuddy--icon-warning event-str)))))
  ;; 进程结束前，把 line-buffer 中残余的不完整行刷出
  (when (and (stringp codebuddy--line-buffer)
             (not (string-empty-p codebuddy--line-buffer)))
    (codebuddy--dispatch-line codebuddy--line-buffer))
  ;; 保存 AI 回复到数据库
  (when (and codebuddy--db
             (not (string-empty-p codebuddy--current-response)))
    (codebuddy--db-save-message "assistant"
                                (string-trim codebuddy--current-response)
                                codebuddy--last-tokens-used))
  ;; 显示 token 统计
  (when codebuddy--db
    (codebuddy--insert-token-stats))
  ;; 重置状态
  (setq codebuddy--process nil)
  (setq codebuddy--is-thinking nil)
  (setq codebuddy--output-buffer "")
  (setq codebuddy--line-buffer "")
  (setq codebuddy--output-phase 'header)
  (setq codebuddy--current-response "")
  (setq codebuddy--last-tokens-used nil)
  ;; 对齐所有 Markdown 表格
  (codebuddy--align-tables)
  ;; 插入新的 prompt
  (codebuddy--insert-prompt))

;; ============================================================
;; 窗口布局
;; ============================================================

(defvar codebuddy--saved-window-config nil
  "启动 codebuddy-chat 前保存的窗口配置，关闭时恢复.")

(defun codebuddy--setup-layout ()
  "设置左右分屏布局：左侧 Chat buffer，右侧 Thinking buffer.
隐藏所有其他 buffer."
  ;; 保存当前窗口配置（仅首次）
  (unless codebuddy--saved-window-config
    (setq codebuddy--saved-window-config (current-window-configuration)))
  ;; 清除所有窗口，独占 frame
  (delete-other-windows)
  ;; 左侧: Chat buffer
  (switch-to-buffer (get-buffer-create codebuddy-chat-buffer-name))
  (goto-char (point-max))
  ;; 右侧: Thinking buffer
  (let ((right-win (split-window-right)))
    (set-window-buffer right-win (get-buffer-create codebuddy-thinking-buffer-name))
    ;; 让 thinking buffer 自动滚动到底部
    (with-selected-window right-win
      (goto-char (point-max)))))

;; ============================================================
;; 交互命令
;; ============================================================

;;;###autoload
(defun codebuddy-chat ()
  "启动 CodeBuddy Chat.
弹出全局输入 buffer，选择工作路径后进入交互模式."
  (interactive)
  ;; 如果 buffer 已存在且有绑定路径，直接切过去
  (if (and (get-buffer codebuddy-chat-buffer-name)
           codebuddy--work-directory)
      (codebuddy--setup-layout)
    ;; 否则新建
    (let ((dir (read-directory-name "选择工作目录: " nil nil t)))
      (setq codebuddy--work-directory (expand-file-name dir))
      ;; 初始化 SQLite 数据库
      (codebuddy--db-init)
      (codebuddy--db-ensure-session)
      (let ((buf (get-buffer-create codebuddy-chat-buffer-name)))
        (with-current-buffer buf
          (codebuddy-chat-mode)
          (codebuddy--insert-header)
          ;; 显示会话恢复信息
          (let* ((stats (codebuddy--db-get-session-stats))
                 (rounds (or (nth 0 stats) 0)))
            (when (> rounds 0)
              (let ((inhibit-read-only t))
                (insert (format "\n> %s 已恢复会话 #%d (%d 轮历史对话)\n"
                                codebuddy--icon-history
                                codebuddy--current-session-id
                                rounds)))))
          (codebuddy--insert-prompt)))
      ;; 初始化 thinking buffer
      (let ((thinking-buf (get-buffer-create codebuddy-thinking-buffer-name)))
        (with-current-buffer thinking-buf
          (special-mode)
          (setq-local buffer-read-only nil)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize (format "%s CodeBuddy Thinking\n\n"
                                        codebuddy--icon-thinking)
                                'face 'codebuddy-status-face))
            (insert (propertize "等待输入...\n" 'face 'codebuddy-thinking-face)))))
      ;; 设置左右分屏布局
      (codebuddy--setup-layout))))

(defun codebuddy-send-input ()
  "发送用户输入到 codex-internal exec.
支持以 / 开头的魔法指令（如 /new /tokens /help 等）."
  (interactive)
  ;; 如果正在执行，不允许再次发送
  (when (and codebuddy--process (process-live-p codebuddy--process))
    (user-error "CodeBuddy 正在执行中，请等待完成或 C-c C-c 中断"))
  (let ((input (codebuddy--get-user-input)))
    (when (string-empty-p input)
      (user-error "请输入问题"))
    ;; 保存到输入历史（包括指令）
    (push input codebuddy--history)
    (setq codebuddy--history-index -1)
    ;; 检查是否为魔法指令
    (if (and (string-prefix-p "/" input)
             (codebuddy--handle-slash-command input))
        ;; 指令已处理，不需要调用 codex
        (message "CodeBuddy: 指令已执行")
      ;; 正常消息：保存到 DB 并发送
      (when codebuddy--db
        (codebuddy--db-save-message "user" input))
      ;; 标记输入为只读
      (with-current-buffer (get-buffer codebuddy-chat-buffer-name)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format "\n\n---\n\n**%s 正在执行** `[%s]`\n\n"
                          codebuddy--icon-loading
                          (truncate-string-to-width input 40 nil nil "...")))))
      ;; 清空 thinking buffer
      (codebuddy--clear-thinking-buffer)
      ;; 重置状态
      (setq codebuddy--output-buffer "")
      (setq codebuddy--line-buffer "")
      (setq codebuddy--is-thinking nil)
      (setq codebuddy--output-phase 'header)
      (setq codebuddy--current-response "")
      (setq codebuddy--last-tokens-used nil)
      ;; 构造带上下文的 prompt
      (let* ((context-prompt (if codebuddy--db
                                 (codebuddy--build-context-prompt input)
                               input))
             (args (append codebuddy-default-args
                           (list "-C" codebuddy--work-directory
                                 context-prompt)))
             (process-environment (cons "TERM=dumb" process-environment))
             (proc (make-process
                    :name "codebuddy"
                    :buffer nil
                    :command (cons codebuddy-executable args)
                    :connection-type 'pipe
                    :filter #'codebuddy--process-filter
                    :sentinel #'codebuddy--process-sentinel
                    :noquery t)))
        (setq codebuddy--process proc)
        (message "CodeBuddy: 已提交 \"%s\""
                 (truncate-string-to-width input 50 nil nil "..."))))))

(defun codebuddy-interrupt ()
  "中断当前正在运行的 codex-internal 进程."
  (interactive)
  (if (and codebuddy--process (process-live-p codebuddy--process))
      (progn
        (interrupt-process codebuddy--process)
        (message "CodeBuddy: 已发送中断信号"))
    (message "CodeBuddy: 没有正在运行的进程")))

(defun codebuddy-kill-chat ()
  "关闭 CodeBuddy Chat，清理所有资源并恢复之前的窗口布局."
  (interactive)
  (when (and codebuddy--process (process-live-p codebuddy--process))
    (kill-process codebuddy--process))
  ;; 关闭数据库
  (codebuddy--db-close)
  (setq codebuddy--current-session-id nil)
  (setq codebuddy--process nil)
  (setq codebuddy--work-directory nil)
  (setq codebuddy--output-buffer "")
  (setq codebuddy--line-buffer "")
  (setq codebuddy--is-thinking nil)
  (setq codebuddy--current-response "")
  (setq codebuddy--last-tokens-used nil)
  (when-let* ((buf (get-buffer codebuddy-thinking-buffer-name)))
    (kill-buffer buf))
  (when-let* ((buf (get-buffer codebuddy-chat-buffer-name)))
    (kill-buffer buf))
  ;; 恢复之前的窗口配置
  (when codebuddy--saved-window-config
    (condition-case nil
        (set-window-configuration codebuddy--saved-window-config)
      (error nil))
    (setq codebuddy--saved-window-config nil))
  (message "CodeBuddy: 已关闭"))

(defun codebuddy-clear-chat ()
  "清除 chat buffer 内容，保留工作目录绑定."
  (interactive)
  (when-let* ((buf (get-buffer codebuddy-chat-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (codebuddy--insert-header)
        (codebuddy--insert-prompt))))
  (codebuddy--clear-thinking-buffer)
  (message "CodeBuddy: 已清屏"))

(defun codebuddy-change-directory ()
  "切换工作目录，同时切换到新目录的数据库."
  (interactive)
  (let ((dir (read-directory-name "切换工作目录: "
                                  codebuddy--work-directory nil t)))
    (setq codebuddy--work-directory (expand-file-name dir))
    ;; 重新初始化 DB（新目录的 .codebuddy/chat.db）
    (codebuddy--db-init)
    (codebuddy--db-ensure-session)
    (let* ((stats (codebuddy--db-get-session-stats))
           (rounds (or (nth 0 stats) 0)))
      (codebuddy--append-to-chat
       (format "\n\n---\n\n%s 工作目录已切换到 `%s`\n"
               codebuddy--icon-folder codebuddy--work-directory))
      (when (> rounds 0)
        (codebuddy--append-to-chat
         (format "> %s 已恢复会话 #%d (%d 轮历史对话)\n"
                 codebuddy--icon-history
                 codebuddy--current-session-id rounds)))
      (codebuddy--insert-prompt)
      (message "CodeBuddy: 工作目录已切换到 %s" codebuddy--work-directory))))

(defun codebuddy-history-prev ()
  "切换到上一条历史输入."
  (interactive)
  (when codebuddy--history
    (setq codebuddy--history-index
          (min (1+ codebuddy--history-index)
               (1- (length codebuddy--history))))
    (codebuddy--replace-input (nth codebuddy--history-index codebuddy--history))))

(defun codebuddy-history-next ()
  "切换到下一条历史输入."
  (interactive)
  (when (> codebuddy--history-index 0)
    (setq codebuddy--history-index (1- codebuddy--history-index))
    (codebuddy--replace-input (nth codebuddy--history-index codebuddy--history))))

(defun codebuddy--replace-input (text)
  "替换当前输入区域为 TEXT."
  (with-current-buffer (get-buffer codebuddy-chat-buffer-name)
    (save-excursion
      (goto-char (point-max))
      (let ((end (point)))
        (when (search-backward codebuddy--prompt-string nil t)
          (forward-char (length codebuddy--prompt-string))
          (let ((inhibit-read-only t))
            (delete-region (point) end)
            (insert text)))))
    (goto-char (point-max))))

(provide 'init-codebuddy)
;;; init-codebuddy.el ends here
