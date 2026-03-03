;;; init-codebuddy.el --- CodeBuddy chat interface for Emacs -*- lexical-binding: t; -*-

;; Author: chenlong
;; Description: 通过 codex-internal exec 与 CodeBuddy 交互的 Emacs 模块
;; 架构: 三窗口布局
;;   左上: Result buffer (org-mode, 只读, AI 结果展示)
;;   左下: Chat buffer (vterm, 交互式输入)
;;   右侧: Thinking buffer (org-mode, 思考过程)

;;; Code:

(require 'org)
(require 'nerd-icons)
(require 'vterm)

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
  "VTerm 输入 buffer 名称."
  :type 'string
  :group 'codebuddy)

(defcustom codebuddy-result-buffer-name "*CodeBuddy-Result*"
  "AI 结果展示 buffer 名称."
  :type 'string
  :group 'codebuddy)

(defcustom codebuddy-max-context-chars 80000
  "上下文拼接的最大字符数 (约 20K tokens).
超出时从最早的对话轮次开始裁剪."
  :type 'integer
  :group 'codebuddy)

(defcustom codebuddy-enable-history nil
  "是否开启会话历史功能（SQLite 持久化）.
默认关闭。可在项目的 .dir-locals.el 中设置为 t 来开启:

  ((nil . ((codebuddy-enable-history . t))))

开启后会在工作目录下创建 .codebuddy/chat.db 存储对话历史，
支持多轮上下文、会话管理等功能."
  :type 'boolean
  :group 'codebuddy
  :safe #'booleanp)

;; 显式标记为 safe local variable，确保 .dir-locals.el 不弹安全确认
(put 'codebuddy-enable-history 'safe-local-variable #'booleanp)

(defcustom codebuddy-db-dir-name ".codebuddy"
  "工作目录下存放 SQLite 数据库的子目录名."
  :type 'string
  :group 'codebuddy)

(defcustom codebuddy-chat-window-ratio 0.65
  "左侧窗口占 frame 宽度的比例 (0.0-1.0).
剩余部分分配给 Thinking buffer."
  :type 'float
  :group 'codebuddy)

(defcustom codebuddy-result-height-ratio 0.70
  "Result buffer 占左侧高度的比例 (0.0-1.0).
剩余部分分配给 Chat (vterm) buffer."
  :type 'float
  :group 'codebuddy)

(defcustom codebuddy-context-full-rounds 3
  "保留完整内容的最近对话轮数.
超出此轮数的早期对话会被压缩为摘要以节省 token."
  :type 'integer
  :group 'codebuddy)

(defcustom codebuddy-context-summary-max-chars 200
  "早期对话摘要的最大字符数（每条消息）."
  :type 'integer
  :group 'codebuddy)

(defcustom codebuddy-input-script
  (expand-file-name "codebuddy-input.sh"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory)))
  "VTerm 输入循环脚本路径."
  :type 'string
  :group 'codebuddy)

;; ============================================================
;; 内部变量
;; ============================================================

(defvar codebuddy--work-directory nil
  "当前 chat buffer 绑定的工作路径.")

(defvar codebuddy--process nil
  "当前运行的 codex-internal 进程.")

(defvar codebuddy--vterm-process nil
  "VTerm 中运行的 shell 进程（用于发送 SIGUSR1 信号）.")

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
  (nerd-icons-octicon "nf-oct-hubot" :face '(:foreground "#61afef"))
  "Robot 图标 (标题/AI 回复).")

(defvar codebuddy--icon-user
  (nerd-icons-faicon "nf-fa-user" :face '(:foreground "#98c379"))
  "User 图标 (用户输入 prompt).")

(defvar codebuddy--icon-folder
  (nerd-icons-faicon "nf-fa-folder_open" :face '(:foreground "#e5c07b"))
  "文件夹图标 (工作目录).")

(defvar codebuddy--icon-send
  (nerd-icons-faicon "nf-fa-paper_plane" :face '(:foreground "#61afef"))
  "发送图标 (RET).")

(defvar codebuddy--icon-stop
  (nerd-icons-faicon "nf-fa-stop_circle" :face '(:foreground "#e06c75"))
  "停止图标 (C-c C-c).")

(defvar codebuddy--icon-close
  (nerd-icons-faicon "nf-fa-times_circle" :face '(:foreground "#e06c75"))
  "关闭图标 (C-c C-k).")

(defvar codebuddy--icon-directory
  (nerd-icons-faicon "nf-fa-exchange" :face '(:foreground "#e5c07b"))
  "切换目录图标 (C-c C-d).")

(defvar codebuddy--icon-check
  (nerd-icons-faicon "nf-fa-check_circle" :face '(:foreground "#98c379"))
  "完成图标.")

(defvar codebuddy--icon-error
  (nerd-icons-faicon "nf-fa-times_circle" :face '(:foreground "#e06c75"))
  "错误/中断图标.")

(defvar codebuddy--icon-warning
  (nerd-icons-faicon "nf-fa-exclamation_triangle" :face '(:foreground "#e5c07b"))
  "警告图标.")

(defvar codebuddy--icon-loading
  (nerd-icons-faicon "nf-fa-spinner" :face '(:foreground "#c678dd"))
  "加载中图标.")

(defvar codebuddy--icon-thinking
  (nerd-icons-octicon "nf-oct-light_bulb" :face '(:foreground "#5c6370"))
  "思考图标.")

(defvar codebuddy--icon-keyboard
  (nerd-icons-faicon "nf-fa-keyboard_o" :face '(:foreground "#abb2bf"))
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
;; Dir-locals 检测
;; ============================================================

(defun codebuddy--check-dir-local-history ()
  "检查工作目录的 .dir-locals.el 中是否设置了 codebuddy-enable-history 为 t.
返回 non-nil 表示开启会话历史."
  (let ((dir-locals-file (expand-file-name ".dir-locals.el"
                                           codebuddy--work-directory)))
    (if (file-exists-p dir-locals-file)
        ;; 用临时 buffer 加载 dir-locals 并读取变量值
        (with-temp-buffer
          (setq default-directory codebuddy--work-directory)
          (hack-dir-local-variables-non-file-buffer)
          (alist-get 'codebuddy-enable-history
                     dir-local-variables-alist))
      ;; 无 .dir-locals.el，返回全局默认值
      codebuddy-enable-history)))

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

(defun codebuddy--summarize-content (content max-chars)
  "将 CONTENT 压缩为不超过 MAX-CHARS 字符的摘要.
保留开头内容，超出部分截断并加省略标记."
  (if (<= (length content) max-chars)
      content
    (concat (substring content 0 (min max-chars (length content))) "...")))

(defun codebuddy--build-context-prompt (new-input)
  "基于当前会话历史和 NEW-INPUT 构建完整的上下文 prompt.
最近 `codebuddy-context-full-rounds' 轮对话保留完整内容，
更早的对话压缩为摘要。会根据 `codebuddy-max-context-chars' 裁剪."
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
    ;; 分离：最近 N 轮保留完整，更早的压缩为摘要
    (let* ((total (length pairs))
           (full-start (max 0 (- total codebuddy-context-full-rounds)))
           (summary-pairs (seq-subseq pairs 0 full-start))
           (full-pairs (seq-subseq pairs full-start))
           ;; 压缩早期对话
           (compressed-pairs
            (mapcar (lambda (p)
                      (cons (codebuddy--summarize-content
                             (car p) codebuddy-context-summary-max-chars)
                            (when (cdr p)
                              (codebuddy--summarize-content
                               (cdr p) codebuddy-context-summary-max-chars))))
                    summary-pairs))
           ;; 合并
           (all-pairs (append compressed-pairs full-pairs)))
      ;; 裁剪：从最早的对话开始删除，直到总字符数在预算内
      (let ((total-chars (+ (length new-input)
                            (apply #'+ (mapcar (lambda (p)
                                                 (+ (length (car p))
                                                    (length (or (cdr p) ""))))
                                               all-pairs)))))
        (while (and all-pairs (> total-chars codebuddy-max-context-chars))
          (let ((removed (pop all-pairs)))
            (setq total-chars (- total-chars
                                 (length (car removed))
                                 (length (or (cdr removed) ""))))
            (setq skipped (1+ skipped)))))
      ;; 拼接 prompt
      (let ((parts '())
            (summary-count (length compressed-pairs)))
        (when (> (length all-pairs) 0)
          (push "[以下是之前的对话上下文，请基于此继续回答]\n" parts)
          (when (> skipped 0)
            (push (format "[已省略更早的 %d 轮对话]\n\n" skipped) parts))
          (let ((idx 1))
            (dolist (pair all-pairs)
              (let ((is-summary (and (> summary-count 0)
                                     (<= idx (- summary-count skipped)))))
                (push (format "%s### 对话 %d\n**User**: %s\n"
                              (if is-summary "[摘要] " "")
                              idx (car pair))
                      parts)
                (when (cdr pair)
                  (push (format "**Assistant**: %s\n" (cdr pair)) parts))
                (push "\n" parts)
                (setq idx (1+ idx)))))
          (push "---\n[当前问题]\n" parts))
        (push new-input parts)
        (apply #'concat (nreverse parts))))))

;; ============================================================
;; 魔法指令系统
;; ============================================================

(defvar codebuddy--icon-magic
  (nerd-icons-faicon "nf-fa-magic" :face '(:foreground "#c678dd"))
  "魔法指令图标.")

(defvar codebuddy--icon-info
  (nerd-icons-faicon "nf-fa-info_circle" :face '(:foreground "#61afef"))
  "信息图标.")

(defvar codebuddy--icon-chart
  (nerd-icons-faicon "nf-fa-bar_chart" :face '(:foreground "#98c379"))
  "统计图标.")

(defvar codebuddy--icon-history
  (nerd-icons-faicon "nf-fa-history" :face '(:foreground "#e5c07b"))
  "历史图标.")

(defvar codebuddy--icon-new
  (nerd-icons-faicon "nf-fa-plus_circle" :face '(:foreground "#98c379"))
  "新建图标.")

(defvar codebuddy--icon-delete
  (nerd-icons-faicon "nf-fa-trash" :face '(:foreground "#e06c75"))
  "删除图标.")

(defvar codebuddy--icon-broom
  (nerd-icons-faicon "nf-fa-eraser" :face '(:foreground "#e5c07b"))
  "清理图标.")

(defvar codebuddy--slash-commands
  '("/new" "/clear" "/tokens" "/history" "/session " "/delete " "/delete all" "/purge" "/cd " "/cd" "/help")
  "所有可用的魔法指令列表.
带尾部空格的表示需要参数（如 /session N, /delete N, /cd PATH）.")

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
     ;; /cd PATH - 切换工作目录
     ((string-match "^/cd\\s-+\\(.+\\)$" cmd)
      (codebuddy--cmd-cd (string-trim (match-string 1 cmd)))
      t)
     ;; /cd - 无参数，交互式选择目录
     ((string= cmd "/cd")
      (codebuddy--cmd-cd nil)
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
  (codebuddy--append-to-result
   (format "\n\n-----\n\n* %s 新会话\n\n已开启新的会话 (ID: %d)，上下文已重置。\n"
           codebuddy--icon-new
           codebuddy--current-session-id)))

(defun codebuddy--cmd-clear ()
  "执行 /clear: 清屏 + 新建会话."
  (codebuddy--db-new-session)
  (when-let* ((buf (get-buffer codebuddy-result-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (codebuddy--insert-result-header))))
  (codebuddy--clear-thinking-buffer)
  (codebuddy--append-to-result
   (format "\n%s 已清屏并开启新会话 (ID: %d)\n"
           codebuddy--icon-new
           codebuddy--current-session-id)))

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
    (codebuddy--append-to-result
     (format "\n\n-----\n\n%s *Token 使用统计*\n\n| 项目 | 数值 |\n|-+-|\n| 当前会话 | #%d |\n| 对话轮数 | %d 轮 |\n| 上下文字符数 | %s |\n| 估算 Token 数 | ~%s |\n| 实际消耗 Token | %s |\n| 上下文预算 | %d%% (%s / %s chars) |\n\n"
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
  (codebuddy--align-tables))

(defun codebuddy--cmd-history ()
  "执行 /history: 列出所有历史会话."
  (let ((sessions (codebuddy--db-list-sessions)))
    (if (null sessions)
        (codebuddy--append-to-result
         (format "\n\n%s 暂无历史会话记录。\n" codebuddy--icon-info))
      (codebuddy--append-to-result
       (format "\n\n-----\n\n%s *历史会话列表*\n\n| ID | 标题 | 创建时间 | 状态 | 消息数 |\n|-+-+-+-+-|\n"
               codebuddy--icon-history))
      (dolist (s sessions)
        (let ((id (nth 0 s))
              (title (or (nth 1 s) "(未命名)"))
              (created (nth 2 s))
              (active (nth 3 s))
              (msg-count (nth 4 s)))
          (codebuddy--append-to-result
           (format "| %d | %s | %s | %s | %d |\n"
                   id title created
                   (if (= active 1) "*当前*" "-")
                   msg-count))))
      (codebuddy--append-to-result
       "\n使用 =/session N= 切换到指定会话\n\n")))
  (codebuddy--align-tables))

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
            (codebuddy--append-to-result
             (format "\n\n-----\n\n%s 已切换到会话 #%d (%d 轮对话，~%s tokens 上下文)\n"
                     codebuddy--icon-history
                     target-id
                     (or (nth 0 stats) 0)
                     (codebuddy--format-number (or (nth 2 stats) 0))))))
      (codebuddy--append-to-result
       (format "\n\n%s 会话 #%d 不存在，请用 =/history= 查看可用会话。\n"
               codebuddy--icon-warning target-id)))))

(defun codebuddy--cmd-delete (target-id)
  "执行 /delete N: 删除指定会话 TARGET-ID."
  (let ((exists (caar (sqlite-select
                       codebuddy--db
                       "SELECT COUNT(*) FROM sessions WHERE id = ?"
                       (list target-id)))))
    (if (and exists (> exists 0))
        (let ((msg-count (codebuddy--db-delete-session target-id)))
          (codebuddy--append-to-result
           (format "\n\n-----\n\n%s 已删除会话 #%d（%d 条消息）\n"
                   codebuddy--icon-delete target-id msg-count))
          (codebuddy--append-to-result
           (format "当前活跃会话: #%d\n" codebuddy--current-session-id)))
      (codebuddy--append-to-result
       (format "\n\n%s 会话 #%d 不存在，请用 =/history= 查看可用会话。\n"
               codebuddy--icon-warning target-id)))))

(defun codebuddy--cmd-delete-all ()
  "执行 /delete all: 删除所有会话."
  (let ((count (codebuddy--db-delete-all-sessions)))
    (codebuddy--append-to-result
     (format "\n\n-----\n\n%s 已删除全部 %d 个会话，已自动开启新会话 #%d\n"
             codebuddy--icon-delete count codebuddy--current-session-id))))

(defun codebuddy--cmd-purge ()
  "执行 /purge: 删除所有非活跃的旧会话，只保留当前会话."
  (let ((count (codebuddy--db-purge-inactive)))
    (if (> count 0)
        (codebuddy--append-to-result
         (format "\n\n-----\n\n%s 已清理 %d 个旧会话，仅保留当前会话 #%d\n"
                 codebuddy--icon-broom count codebuddy--current-session-id))
      (codebuddy--append-to-result
       (format "\n\n%s 没有需要清理的旧会话。\n" codebuddy--icon-info)))))

(defun codebuddy--cmd-cd (path)
  "执行 /cd: 切换工作目录.
PATH 为目标路径，支持 ~ 展开。若 PATH 为 nil 则显示当前目录和用法提示."
  (if (null path)
      ;; 无参数：显示当前目录和用法
      (codebuddy--append-to-result
       (format "\n\n%s 当前工作目录: =%s=\n\n用法: =/cd PATH= （支持 =~= 展开）\n示例: =/cd ~/projects/myapp=\n\n在 Result buffer 中也可用 =C-c C-d= 交互式选择目录。\n"
               codebuddy--icon-folder codebuddy--work-directory))
    (let ((dir (expand-file-name (substitute-in-file-name path))))
      (if (file-directory-p dir)
          (progn
            (setq codebuddy--work-directory (expand-file-name dir))
            ;; 关闭旧 DB
            (codebuddy--db-close)
            ;; 根据新目录的 dir-locals 决定是否开启历史
            (let ((enable-hist (codebuddy--check-dir-local-history)))
              (if enable-hist
                  (progn
                    (codebuddy--db-init)
                    (codebuddy--db-ensure-session))
                (setq codebuddy--db nil)
                (setq codebuddy--current-session-id nil)))
            (codebuddy--append-to-result
             (format "\n\n-----\n\n%s 工作目录已切换到 =%s=%s\n"
                     codebuddy--icon-folder codebuddy--work-directory
                     (if codebuddy--db " (会话历史: 开启)" " (会话历史: 关闭)")))
            (when codebuddy--db
              (let* ((stats (codebuddy--db-get-session-stats))
                     (rounds (or (nth 0 stats) 0)))
                (when (> rounds 0)
                  (codebuddy--append-to-result
                   (format "%s 已恢复会话 #%d (%d 轮历史对话)\n"
                           codebuddy--icon-history
                           codebuddy--current-session-id rounds)))))
            (message "CodeBuddy: 工作目录已切换到 %s" codebuddy--work-directory))
        (codebuddy--append-to-result
         (format "\n\n%s 目录不存在: =%s=\n"
                 codebuddy--icon-warning path))))))

(defun codebuddy--cmd-help ()
  "执行 /help: 显示所有可用指令."
  (codebuddy--append-to-result
   (format "\n\n-----\n\n%s *可用指令*\n\n| 指令 | 功能 |\n|-+-|\n| =/new= | 新建会话（保留 buffer 内容） |\n| =/clear= | 清屏 + 新建会话 |\n| =/tokens= | 显示当前上下文 Token 使用统计 |\n| =/history= | 列出所有历史会话 |\n| =/session N= | 切换到第 N 个历史会话 |\n| =/delete N= | 删除指定会话 #N 及其所有消息 |\n| =/delete all= | 删除所有会话（清空历史） |\n| =/purge= | 清理旧会话，只保留当前会话 |\n| =/cd PATH= | 切换工作目录（支持 ~，无参数显示当前目录） |\n| =/help= | 显示此帮助 |\n\n"
           codebuddy--icon-magic))
  (codebuddy--align-tables))

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
  "在 result buffer 中插入本次交互的 token 统计摘要."
  (let* ((stats (codebuddy--db-get-session-stats))
         (rounds (or (nth 0 stats) 0))
         (chars (or (nth 1 stats) 0))
         (est-tokens (or (nth 2 stats) 0)))
    (codebuddy--append-to-result
     (format "\n%s 上下文: ~%s tokens (%s chars, %d轮)%s\n"
             codebuddy--icon-chart
             (codebuddy--format-number est-tokens)
             (codebuddy--format-number chars)
             rounds
             (if codebuddy--last-tokens-used
                 (format " | 本次消耗: %s tokens"
                         (codebuddy--format-number codebuddy--last-tokens-used))
               "")))))

;; ============================================================
;; Result buffer (org-mode, 只读)
;; ============================================================

(define-derived-mode codebuddy-result-mode org-mode "CB-Result"
  "CodeBuddy Result 展示模式.
基于 org-mode，只读，用于展示 AI 回复结果."
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (visual-line-mode 1))

(defun codebuddy--insert-result-header ()
  "在 result buffer 插入欢迎头部信息."
  (let ((inhibit-read-only t))
    (insert (format "#+TITLE: %s CodeBuddy Result\n\n"
                    codebuddy--icon-robot))
    (insert (format "- %s 工作目录: =%s=\n"
                    codebuddy--icon-folder
                    codebuddy--work-directory))
    (insert (format "- %s 会话历史: %s\n"
                    codebuddy--icon-info
                    (if codebuddy--db "开启" "关闭（在 .dir-locals.el 中设置 codebuddy-enable-history 为 t 开启）")))
    (insert (format "- %s AI 回复结果将显示在此处\n\n-----\n"
                    codebuddy--icon-info))))

(defun codebuddy--append-to-result (text)
  "在 result buffer 中追加 TEXT，并刷新 font-lock."
  (when-let* ((buf (get-buffer codebuddy-result-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (start (point-max)))
        (goto-char start)
        (insert text)
        (font-lock-flush start (point-max)))))
  ;; 自动滚动 result buffer
  (when-let* ((win (get-buffer-window codebuddy-result-buffer-name t)))
    (with-current-buffer (get-buffer codebuddy-result-buffer-name)
      (set-window-point win (point-max))
      (with-selected-window win
        (goto-char (point-max))
        (recenter -1)))))

(defun codebuddy--align-tables ()
  "对齐 result buffer 中所有 org 表格."
  (when-let* ((buf (get-buffer codebuddy-result-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*|" nil t)
            (beginning-of-line)
            (when (org-at-table-p)
              (condition-case nil
                  (org-table-align)
                (error nil)))
            (forward-line 1)))))))

;; ============================================================
;; Thinking buffer (org-mode)
;; ============================================================

(defun codebuddy--append-to-thinking (text)
  "在 thinking buffer 中追加 TEXT."
  (let ((buf (get-buffer-create codebuddy-thinking-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode)
        (setq-local buffer-read-only nil)
        (setq-local truncate-lines nil)
        (setq-local word-wrap t)
        (visual-line-mode 1))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert text)))
    ;; 自动滚动 thinking buffer（如果可见）
    (when-let* ((win (get-buffer-window buf t)))
      (with-current-buffer buf
        (set-window-point win (point-max))
        ;; 确保窗口滚动到底部
        (with-selected-window win
          (goto-char (point-max))
          (recenter -1))))))

(defun codebuddy--clear-thinking-buffer ()
  "清除 thinking buffer 内容并写入新的 org-mode header."
  (let ((buf (get-buffer-create codebuddy-thinking-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode)
        (setq-local buffer-read-only nil)
        (setq-local truncate-lines nil)
        (setq-local word-wrap t)
        (visual-line-mode 1))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: %s CodeBuddy Thinking\n\n"
                        codebuddy--icon-thinking))))))

;; ============================================================
;; 输出分发（状态机）
;; ============================================================

(defun codebuddy--dispatch-line (line)
  "根据当前状态机阶段分发单行 LINE 到对应 buffer.
各阶段在 thinking buffer 中使用 org-mode 标题分隔，可折叠."
  (let ((trimmed (string-trim line)))
    (cond
     ;; ---- 阶段切换检测 ----

     ;; 分隔线 --------: header 阶段标记
     ((string-match-p "^--------$" trimmed)
      (setq codebuddy--output-phase 'header))

     ;; "user" 独占一行 -> 进入 user 阶段
     ((and (string= "user" trimmed)
           (memq codebuddy--output-phase '(header user)))
      (setq codebuddy--output-phase 'user)
      (codebuddy--append-to-thinking "\n* User Prompt\n"))

     ;; "thinking" 独占一行 -> 进入 thinking 阶段
     ((string= "thinking" trimmed)
      (setq codebuddy--output-phase 'thinking)
      (codebuddy--append-to-thinking "\n* Thinking\n"))

     ;; "exec" 开头 -> 进入 exec 阶段 (工具调用)
     ((string-match-p "^exec$" trimmed)
      (setq codebuddy--output-phase 'exec)
      (codebuddy--append-to-thinking "\n* Exec (Tool Call)\n"))

     ;; "mcp" 开头 -> mcp 信息，归入 thinking
     ((string-match-p "^mcp " trimmed)
      (codebuddy--append-to-thinking (concat "** MCP: " trimmed "\n")))

     ;; "codex" 独占一行 -> 进入 codex 阶段（结果输出到 result buffer）
     ((string= "codex" trimmed)
      (setq codebuddy--output-phase 'codex)
      (codebuddy--append-to-thinking "\n* Result (→ Result Buffer)\n"))

     ;; "tokens used" -> 进入 tokens 阶段
     ((string-match-p "^tokens used$" trimmed)
      (setq codebuddy--output-phase 'tokens)
      (codebuddy--append-to-thinking "\n* Tokens Used\n"))

     ;; ---- 各阶段内容分发 ----

     ;; tokens 阶段: 统计信息 -> thinking
     ((eq codebuddy--output-phase 'tokens)
      (codebuddy--parse-tokens-line line)
      (codebuddy--append-to-thinking (concat line "\n")))

     ;; codex 阶段: 最终结果 -> result buffer
     ((eq codebuddy--output-phase 'codex)
      (setq codebuddy--current-response
            (concat codebuddy--current-response line "\n"))
      (codebuddy--append-to-result (concat line "\n")))

     ;; header/user/thinking/exec 阶段: 全部 -> thinking buffer
     ((memq codebuddy--output-phase '(header user thinking exec))
      (codebuddy--append-to-thinking (concat line "\n")))

     ;; 未知阶段，默认输出到 thinking
     (t
      (codebuddy--append-to-thinking (concat line "\n"))))))

(defun codebuddy--process-filter (_proc output)
  "处理 codex-internal 的输出 OUTPUT.
正确处理跨批次的不完整行."
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
      (codebuddy--dispatch-line line))))

(defun codebuddy--process-sentinel (_proc event)
  "codex-internal 进程结束时的处理函数."
  (let ((event-str (string-trim event)))
    (cond
     ((string-match-p "finished" event-str)
      (codebuddy--append-to-result
       (format "\n\n-----\n\n*%s 执行完成*\n" codebuddy--icon-check)))
     ((string-match-p "\\(killed\\|interrupt\\)" event-str)
      (codebuddy--append-to-result
       (format "\n\n-----\n\n*%s 已中断*\n" codebuddy--icon-error)))
     (t
      (codebuddy--append-to-result
       (format "\n\n-----\n\n*%s 进程退出: %s*\n"
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
  ;; 通知 vterm shell 可以继续输入
  (codebuddy--vterm-notify-ready))

;; ============================================================
;; VTerm 集成
;; ============================================================

(defun codebuddy--vterm-send (input)
  "VTerm 回调：接收用户输入 INPUT 并处理.
由 vterm escape sequence 触发."
  ;; 确保三窗口布局（用户可能已切换/关闭了某个窗口）
  (codebuddy--ensure-layout)
  ;; 保存到输入历史
  (push input codebuddy--history)
  (setq codebuddy--history-index -1)
  ;; 在 result buffer 中显示用户问题
  (codebuddy--append-to-result
   (format "\n\n-----\n\n*%s You>* %s\n\n"
           codebuddy--icon-user input))
  ;; 检查是否为魔法指令
  (if (and (string-prefix-p "/" input)
           (codebuddy--handle-slash-command input))
      ;; 指令已处理
      (progn
        (message "CodeBuddy: 指令已执行")
        (codebuddy--vterm-notify-ready))
    ;; 正常消息：保存到 DB 并发送
    (when codebuddy--db
      (codebuddy--db-save-message "user" input))
    (codebuddy--append-to-result
     (format "*%s 正在执行* =[%s]=\n\n"
             codebuddy--icon-loading
             (truncate-string-to-width input 40 nil nil "...")))
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
           (process-environment (append '("TERM=dumb" "NO_COLOR=1")
                                        process-environment))
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
               (truncate-string-to-width input 50 nil nil "...")))))

(defun codebuddy--vterm-interrupt ()
  "VTerm 回调：中断当前正在运行的 codex-internal 进程."
  (if (and codebuddy--process (process-live-p codebuddy--process))
      (progn
        (interrupt-process codebuddy--process)
        (message "CodeBuddy: 已发送中断信号"))
    (message "CodeBuddy: 没有正在运行的进程")))

(defun codebuddy--vterm-notify-ready ()
  "通知 vterm shell 进程可以继续接受输入.
通过发送 SIGUSR1 信号."
  (when codebuddy--vterm-process
    (condition-case nil
        (signal-process codebuddy--vterm-process 'sigusr1)
      (error nil))))

(defun codebuddy--init-vterm ()
  "初始化 VTerm Chat buffer.
启动 vterm 并运行 codebuddy-input.sh 脚本."
  (let ((buf (get-buffer-create codebuddy-chat-buffer-name)))
    ;; 如果已有 vterm 进程在运行，直接返回
    (if (and (buffer-live-p buf)
             (get-buffer-process buf))
        buf
      ;; 创建新的 vterm buffer
      (with-current-buffer buf
        (let ((vterm-shell (format "/bin/zsh -c '%s'" codebuddy-input-script))
              (vterm-buffer-name codebuddy-chat-buffer-name)
              (vterm-kill-buffer-on-exit nil))
          (vterm-mode)
          ;; 注册 vterm eval 回调
          (setq-local vterm-eval-cmds
                      (append vterm-eval-cmds
                              '(("codebuddy--vterm-send" codebuddy--vterm-send)
                                ("codebuddy--vterm-interrupt" codebuddy--vterm-interrupt))))
          ;; 记录 vterm 内部的 shell 进程
          (setq codebuddy--vterm-process (get-buffer-process buf))))
      buf)))

;; ============================================================
;; 窗口布局
;; ============================================================

(defvar codebuddy--saved-window-config nil
  "启动 codebuddy-chat 前保存的窗口配置，关闭时恢复.")

(defun codebuddy--layout-ok-p ()
  "检测当前 frame 是否处于 CodeBuddy 三窗口布局.
即 Result / Chat / Thinking 三个 buffer 各自有对应窗口."
  (let ((result-win (get-buffer-window codebuddy-result-buffer-name))
        (chat-win   (get-buffer-window codebuddy-chat-buffer-name))
        (think-win  (get-buffer-window codebuddy-thinking-buffer-name)))
    (and result-win chat-win think-win
         (window-live-p result-win)
         (window-live-p chat-win)
         (window-live-p think-win))))

(defun codebuddy--ensure-layout ()
  "若当前不是三窗口布局则自动恢复."
  (unless (codebuddy--layout-ok-p)
    (codebuddy--setup-layout)))

(defun codebuddy--setup-layout ()
  "设置三窗口布局:
左上: Result buffer (org-mode, 只读)
左下: Chat buffer (vterm, 输入)
右侧: Thinking buffer (org-mode)"
  ;; 保存当前窗口配置（仅首次）
  (unless codebuddy--saved-window-config
    (setq codebuddy--saved-window-config (current-window-configuration)))
  ;; 清除所有窗口，独占 frame
  (delete-other-windows)
  ;; 左侧: Result buffer（占 65% 宽度）
  (switch-to-buffer (get-buffer-create codebuddy-result-buffer-name))
  (goto-char (point-max))
  ;; 右侧: Thinking buffer
  (let* ((left-cols (floor (* (frame-width) codebuddy-chat-window-ratio)))
         (right-win (split-window-right left-cols)))
    (set-window-buffer right-win (get-buffer-create codebuddy-thinking-buffer-name))
    (with-selected-window right-win
      (goto-char (point-max))))
  ;; 左下: Chat (vterm) buffer
  (let* ((result-rows (floor (* (window-height) codebuddy-result-height-ratio)))
         (chat-win (split-window-below result-rows)))
    (set-window-buffer chat-win (get-buffer-create codebuddy-chat-buffer-name))
    ;; 聚焦到 Chat (vterm) 窗口，让用户可以直接输入
    (select-window chat-win)))

;; ============================================================
;; 交互命令
;; ============================================================

;;;###autoload
(defun codebuddy-chat ()
  "启动 CodeBuddy Chat.
弹出三窗口布局，选择工作路径后进入交互模式."
  (interactive)
  ;; 如果 buffer 已存在且有绑定路径，直接切过去
  (if (and (get-buffer codebuddy-result-buffer-name)
           (get-buffer codebuddy-chat-buffer-name)
           codebuddy--work-directory)
      (codebuddy--setup-layout)
    ;; 否则新建
    (let ((dir (read-directory-name "选择工作目录: " nil nil t)))
      (setq codebuddy--work-directory (expand-file-name dir))
      ;; 读取工作目录的 dir-locals 设置，决定是否开启会话历史
      (let ((enable-hist (codebuddy--check-dir-local-history)))
        (if enable-hist
            (progn
              (codebuddy--db-init)
              (codebuddy--db-ensure-session))
          (setq codebuddy--db nil)
          (setq codebuddy--current-session-id nil)))
      ;; 初始化 Result buffer
      (let ((result-buf (get-buffer-create codebuddy-result-buffer-name)))
        (with-current-buffer result-buf
          (codebuddy-result-mode)
          (let ((inhibit-read-only t))
            (codebuddy--insert-result-header)
            ;; 显示会话恢复信息（仅历史功能开启时）
            (when codebuddy--db
              (let* ((stats (codebuddy--db-get-session-stats))
                     (rounds (or (nth 0 stats) 0)))
                (when (> rounds 0)
                  (insert (format "\n%s 已恢复会话 #%d (%d 轮历史对话)\n"
                                  codebuddy--icon-history
                                  codebuddy--current-session-id
                                  rounds))))))))
      ;; 初始化 VTerm Chat buffer
      (codebuddy--init-vterm)
      ;; 初始化 Thinking buffer
      (let ((thinking-buf (get-buffer-create codebuddy-thinking-buffer-name)))
        (with-current-buffer thinking-buf
          (org-mode)
          (setq-local buffer-read-only nil)
          (setq-local truncate-lines nil)
          (setq-local word-wrap t)
          (visual-line-mode 1)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "#+TITLE: %s CodeBuddy Thinking\n\n"
                            codebuddy--icon-thinking))
            (insert "等待输入...\n"))))
      ;; 设置三窗口布局
      (codebuddy--setup-layout))))

(defun codebuddy-interrupt ()
  "中断当前正在运行的 codex-internal 进程."
  (interactive)
  (codebuddy--vterm-interrupt))

(defun codebuddy-kill-chat ()
  "关闭 CodeBuddy Chat，清理所有资源并恢复之前的窗口布局."
  (interactive)
  (when (and codebuddy--process (process-live-p codebuddy--process))
    (kill-process codebuddy--process))
  ;; 关闭数据库
  (codebuddy--db-close)
  (setq codebuddy--current-session-id nil)
  (setq codebuddy--process nil)
  (setq codebuddy--vterm-process nil)
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
  (when-let* ((buf (get-buffer codebuddy-result-buffer-name)))
    (kill-buffer buf))
  ;; 恢复之前的窗口配置
  (when codebuddy--saved-window-config
    (condition-case nil
        (set-window-configuration codebuddy--saved-window-config)
      (error nil))
    (setq codebuddy--saved-window-config nil))
  (message "CodeBuddy: 已关闭"))

(defun codebuddy-clear-chat ()
  "清除 result buffer 内容，保留工作目录绑定."
  (interactive)
  (when-let* ((buf (get-buffer codebuddy-result-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (codebuddy--insert-result-header))))
  (codebuddy--clear-thinking-buffer)
  (message "CodeBuddy: 已清屏"))

(defun codebuddy-change-directory ()
  "切换工作目录，根据新目录的 .dir-locals.el 决定是否开启会话历史."
  (interactive)
  (let ((dir (read-directory-name "切换工作目录: "
                                  codebuddy--work-directory nil t)))
    (setq codebuddy--work-directory (expand-file-name dir))
    ;; 关闭旧 DB
    (codebuddy--db-close)
    ;; 根据新目录的 dir-locals 决定是否开启历史
    (let ((enable-hist (codebuddy--check-dir-local-history)))
      (if enable-hist
          (progn
            (codebuddy--db-init)
            (codebuddy--db-ensure-session))
        (setq codebuddy--db nil)
        (setq codebuddy--current-session-id nil)))
    (codebuddy--append-to-result
     (format "\n\n-----\n\n%s 工作目录已切换到 =%s=%s\n"
             codebuddy--icon-folder codebuddy--work-directory
             (if codebuddy--db " (会话历史: 开启)" " (会话历史: 关闭)")))
    (when codebuddy--db
      (let* ((stats (codebuddy--db-get-session-stats))
             (rounds (or (nth 0 stats) 0)))
        (when (> rounds 0)
          (codebuddy--append-to-result
           (format "%s 已恢复会话 #%d (%d 轮历史对话)\n"
                   codebuddy--icon-history
                   codebuddy--current-session-id rounds)))))
    (message "CodeBuddy: 工作目录已切换到 %s" codebuddy--work-directory)))

;; ============================================================
;; 全局快捷键（在 vterm 外部也可使用）
;; ============================================================

(defvar codebuddy-global-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'codebuddy-interrupt)
    (define-key map (kbd "C-c C-k") #'codebuddy-kill-chat)
    (define-key map (kbd "C-c C-l") #'codebuddy-clear-chat)
    (define-key map (kbd "C-c C-d") #'codebuddy-change-directory)
    map)
  "CodeBuddy 全局快捷键（在 result buffer 中可用）.")

;; 绑定到 result-mode
(define-key codebuddy-result-mode-map (kbd "C-c C-c") #'codebuddy-interrupt)
(define-key codebuddy-result-mode-map (kbd "C-c C-k") #'codebuddy-kill-chat)
(define-key codebuddy-result-mode-map (kbd "C-c C-l") #'codebuddy-clear-chat)
(define-key codebuddy-result-mode-map (kbd "C-c C-d") #'codebuddy-change-directory)

;; ============================================================
;; nerd-icons 集成：为自定义 mode 注册图标
;; ============================================================

(with-eval-after-load 'nerd-icons
  ;; Result buffer: 使用 org 图标
  (add-to-list 'nerd-icons-mode-icon-alist
               '(codebuddy-result-mode nerd-icons-sucicon "nf-custom-orgmode" :face nerd-icons-lgreen)))

(provide 'init-codebuddy)
;;; init-codebuddy.el ends here
