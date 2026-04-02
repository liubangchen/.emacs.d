;; init-markdown.el --- Initialize markdown configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2009-2026 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Markdown configurations.
;;

;;; Code:
(defvar my/markdown-cache-dir
  (expand-file-name "markdown/" (locate-user-emacs-file ".cache/"))
  "Markdown 预览相关文件的缓存目录。")

(defvar my/markdown-css-file
  (expand-file-name "github-markdown.css" my/markdown-cache-dir)
  "本地缓存的 github-markdown CSS 文件路径。")

(defvar my/markdown-template-file
  (expand-file-name "lisp/templates/preview.html" user-emacs-directory)
  "pandoc 自定义 HTML 模板路径（放在 git 跟踪目录下，换机不丢失）。")

(defun my/markdown-ensure-cache-dir ()
  "确保缓存目录存在。"
  (unless (file-directory-p my/markdown-cache-dir)
    (make-directory my/markdown-cache-dir t)))

(defun my/markdown-ensure-css ()
  "确保本地有 github-markdown.css，没有则下载。"
  (my/markdown-ensure-cache-dir)
  (unless (file-exists-p my/markdown-css-file)
    (url-copy-file
     "https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
     my/markdown-css-file t)
    (message "Markdown CSS 已下载到 %s" my/markdown-css-file)))

(defun my/markdown-export-file-name (&optional extension)
  "导出 HTML 到缓存目录，避免污染源文件目录。"
  (my/markdown-ensure-cache-dir)
  (let* ((base (if (buffer-file-name)
                   (file-name-sans-extension
                    (file-name-nondirectory (buffer-file-name)))
                 (md5 (buffer-name))))
         (ext (or extension ".html")))
    (concat my/markdown-cache-dir base ext)))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :hook ((markdown-mode . my/markdown-ensure-css)
         (markdown-mode . my/markdown-setup-table-and-fonts))
  :init
  (setq markdown-command
        (concat "pandoc -f gfm -t html5"
                " --template=" my/markdown-template-file
                " --toc --toc-depth=3"
                " --syntax-highlighting=none"
                " --metadata title=Preview"
                " --css=" my/markdown-css-file))

  (advice-add 'markdown-export-file-name :override
              #'my/markdown-export-file-name)

  :config
  ;; ----- 隐藏 Markup 标记 -----
  ;; 隐藏 **、*、~~、`` 等标记符号，直接显示粗体/斜体/删除线等效果
  ;; 使用 C-c C-x C-m 或 M-x markdown-toggle-markup-hiding 临时切换
  (setq markdown-hide-markup t)

  ;; ----- 表格自动对齐 -----
  ;; markdown-mode 内置：在表格中按 TAB 时自动格式化对齐
  (setq markdown-table-align-p t)

  ;; ----- 代码块强制等宽字体 -----
  ;; 确保代码块/行内代码使用 fixed-pitch，防止混合字体导致 ASCII art 不对齐
  (defun my/markdown-setup-table-and-fonts ()
    "为 Markdown buffer 设置代码块等宽字体。"
    (face-remap-add-relative 'markdown-code-face :inherit 'fixed-pitch)
    (face-remap-add-relative 'markdown-pre-face :inherit 'fixed-pitch)
    (face-remap-add-relative 'markdown-inline-code-face :inherit 'fixed-pitch))

  ;; 优先用 xwidget-webkit
  (when (featurep 'xwidget-internal)
    (setq markdown-live-preview-window-function
          (lambda (file)
            (let ((url (concat "file://" (expand-file-name file))))
              (xwidget-webkit-browse-url url t)
              ;; markdown-live-preview-export 要求返回一个 buffer，
              ;; xwidget-webkit-browse-url 不返回 buffer，需要手动查找。
              (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
                (when buf
                  (my/markdown-preview-setup-keymap buf)
                  ;; 清除 no-delete-other-windows 参数，让 C-x 1 能关闭预览窗口
                  (when-let* ((win (get-buffer-window buf)))
                    (set-window-parameter win 'no-delete-other-windows nil)))
                (or buf (current-buffer)))))))

  ;; ---- xwidget-webkit 键盘焦点问题解决方案 ----
  ;;
  ;; 问题根因：macOS 的 WKWebView 是 NSResponder，会抢占 NSWindow
  ;; 的 firstResponder。导致：
  ;;   1. Cmd+C 等快捷键走 Cocoa responder chain 而非 Emacs keymap
  ;;   2. child frame (vertico-posframe) 弹出时方向键仍被底层 WebKit 吞掉
  ;; 这些事件在 Cocoa 层面就被消费了，Emacs keymap 完全看不到。
  ;;
  ;; 方案：
  ;;   A. 注入 JS 让 Cmd+C 在页面内完成复制（写入系统剪贴板）
  ;;   B. minibuffer 激活时临时移除 xwidget buffer，阻止 WebKit NSView
  ;;      收到键盘事件；minibuffer 退出后恢复

  ;; ---- A. 页面内 JS 处理 Cmd+C 复制 ----
  (defconst my/xwidget-copy-js
    "if (!window.__emacsKeysPatched) {
  window.__emacsKeysPatched = true;
  document.addEventListener('keydown', function(e) {
    /* Cmd+C / Ctrl+C：执行浏览器原生复制 */
    if ((e.metaKey || e.ctrlKey) && e.key === 'c') {
      var sel = window.getSelection();
      if (sel && sel.toString().length > 0) {
        /* 使用 Clipboard API（async，兼容性好） */
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(sel.toString());
        } else {
          document.execCommand('copy');
        }
      }
    }
  }, true);
}"
    "JS：让 Cmd+C 在 WebKit 页面内完成复制到系统剪贴板。")

  (defun my/xwidget-inject-copy-js (xwidget)
    "向 XWIDGET session 注入 Cmd+C 复制 JS。"
    (when (and xwidget (xwidget-live-p xwidget))
      (xwidget-webkit-execute-script xwidget my/xwidget-copy-js)))

  ;; ---- B. minibuffer 期间临时移除 xwidget buffer ----
  ;; 当 vertico-posframe 等 child frame 弹出时，底层 WKWebView 的
  ;; NSView 仍会抢 firstResponder。唯一可靠方案：暂时把 xwidget buffer
  ;; 从窗口移走，让 NSView 不再参与 responder chain。

  (defvar my/xwidget--hidden-windows nil
    "保存被临时替换的 xwidget 窗口信息：((window . buffer) ...)")

  (defun my/xwidget-hide-for-minibuffer ()
    "Minibuffer 激活时，临时将所有 xwidget-webkit 窗口切换到空 buffer。
这样 WKWebView 的 NSView 不在 view hierarchy 中，无法抢 firstResponder。"
    (setq my/xwidget--hidden-windows nil)
    (dolist (win (window-list))
      (let ((buf (window-buffer win)))
        (when (eq 'xwidget-webkit-mode
                  (buffer-local-value 'major-mode buf))
          (push (cons win buf) my/xwidget--hidden-windows)
          ;; 用一个空的占位 buffer 替换，避免 WebKit NSView 参与事件派发
          (let ((placeholder (get-buffer-create " *xwidget-placeholder*")))
            (set-window-buffer win placeholder))))))

  (defun my/xwidget-restore-after-minibuffer ()
    "Minibuffer 退出后，恢复所有被临时替换的 xwidget-webkit 窗口。"
    (dolist (entry my/xwidget--hidden-windows)
      (let ((win (car entry))
            (buf (cdr entry)))
        (when (and (window-live-p win) (buffer-live-p buf))
          (set-window-buffer win buf))))
    (setq my/xwidget--hidden-windows nil)
    ;; 清理占位 buffer
    (let ((ph (get-buffer " *xwidget-placeholder*")))
      (when (and ph (not (get-buffer-window ph)))
        (kill-buffer ph))))

  (add-hook 'minibuffer-setup-hook #'my/xwidget-hide-for-minibuffer)
  (add-hook 'minibuffer-exit-hook  #'my/xwidget-restore-after-minibuffer)

  ;; ---- Keymap 和 minor mode（用于预览 buffer 的鼠标操作等） ----
  (defun my/markdown-preview-jump-back ()
    "从 xwidget 预览窗口跳回 markdown 源 buffer。
若源 buffer 窗口不存在且只有一个窗口，则按方向键原义滚动预览。"
    (interactive)
    (if-let* ((src markdown-live-preview-source-buffer)
              (win (get-buffer-window src)))
        (select-window win)
      (if (one-window-p)
          (pcase (event-basic-type last-command-event)
            ('up    (xwidget-webkit-scroll-down-line))
            ('down  (xwidget-webkit-scroll-up-line))
            ('left  (xwidget-webkit-scroll-backward))
            ('right (xwidget-webkit-scroll-forward))
            (_      nil))
        (other-window -1))))

  (defvar my/markdown-preview-minor-mode-map
    (let ((map (make-sparse-keymap)))
      ;; 鼠标滚轮：在预览区域仍可滚动预览内容
      (define-key map [mouse-4] #'xwidget-webkit-scroll-down-line)
      (define-key map [mouse-5] #'xwidget-webkit-scroll-up-line)
      (define-key map [wheel-up] #'xwidget-webkit-scroll-down)
      (define-key map [wheel-down] #'xwidget-webkit-scroll-up)
      ;; 方向键：如果能到达 Emacs，则跳回源 buffer
      (define-key map [up]    #'my/markdown-preview-jump-back)
      (define-key map [down]  #'my/markdown-preview-jump-back)
      (define-key map [left]  #'my/markdown-preview-jump-back)
      (define-key map [right] #'my/markdown-preview-jump-back)
      ;; q / ESC 跳回
      (define-key map "q" #'my/markdown-preview-jump-back)
      (define-key map [escape] #'my/markdown-preview-jump-back)
      ;; 保留缩放/刷新
      (define-key map "+" #'xwidget-webkit-zoom-in)
      (define-key map "-" #'xwidget-webkit-zoom-out)
      (define-key map "r" #'xwidget-webkit-reload)
      (define-key map "0" #'xwidget-webkit-zoom-in)
      map)
    "Keymap for `my/markdown-preview-minor-mode'.")

  (define-minor-mode my/markdown-preview-minor-mode
    "Minor mode for Markdown xwidget-webkit preview buffers."
    :lighter " MdPrev"
    :keymap my/markdown-preview-minor-mode-map)

  (defun my/markdown-preview-setup-keymap (preview-buf)
    "为 Markdown 预览 buffer 启用 minor mode 并注入复制 JS。"
    (with-current-buffer preview-buf
      (my/markdown-preview-minor-mode 1)
      ;; 注入 Cmd+C 复制支持 JS
      (when-let* ((xw (xwidget-at (point-min))))
        (my/xwidget-inject-copy-js xw))))

  ;; 页面每次 reload 后 JS 会丢失，需要重新注入
  (defun my/xwidget-reinject-on-load (xwidget _event)
    "xwidget 页面加载完成后重新注入 JS。"
    (when (and xwidget (xwidget-live-p xwidget))
      (let ((buf (xwidget-buffer xwidget)))
        (when (and (buffer-live-p buf)
                   (buffer-local-value 'my/markdown-preview-minor-mode buf))
          (my/xwidget-inject-copy-js xwidget)))))

  (advice-add 'xwidget-webkit-callback :after #'my/xwidget-reinject-on-load)

  ;; 禁止 apheleia 格式化 markdown 预览缓存的 HTML
  (defun my/disable-apheleia-in-markdown-cache ()
    (when (and buffer-file-name
               (string-prefix-p
                (expand-file-name my/markdown-cache-dir)
                (expand-file-name buffer-file-name)))
      (apheleia-mode -1)))
  (add-hook 'html-mode-hook #'my/disable-apheleia-in-markdown-cache)
  (add-hook 'mhtml-mode-hook #'my/disable-apheleia-in-markdown-cache))

;; Table of contents
(use-package markdown-toc
  :diminish
  :bind (:map markdown-mode-command-map
         ("r" . markdown-toc-generate-or-refresh-toc))
  :hook markdown-mode
  :init (setq markdown-toc-indentation-space 2
              markdown-toc-header-toc-title "\n## Table of Contents"
              markdown-toc-user-toc-structure-manipulation-fn 'cdr)
  :config
  (with-no-warnings
    (define-advice markdown-toc-generate-toc (:around (fn &rest args) lsp)
      "Generate or refresh toc after disabling lsp."
      (cond
       ((bound-and-true-p eglot--manage-mode)
        (eglot--manage-mode -1)
        (apply fn args)
        (eglot--manage-mode 1))
       ((bound-and-true-p lsp-managed-mode)
        (lsp-managed-mode -1)
        (apply fn args)
        (lsp-managed-mode 1))
       (t
        (apply fn args))))))

;; Preview markdown files
;; @see https://github.com/seagle0128/grip-mode?tab=readme-ov-file#prerequisite
(use-package grip-mode
  :defines markdown-mode-command-map org-mode-map grip-update-after-change grip-use-mdopen
  :functions auth-source-user-and-password
  :autoload grip-mode
  :init
  (with-eval-after-load 'markdown-mode
    (bind-key "g" #'grip-mode markdown-mode-command-map))

  (with-eval-after-load 'org
    (bind-key "C-c C-g" #'grip-mode org-mode-map))

  (setq grip-update-after-change nil)

  ;; mdopen doesn't need credentials, and only support external browsers
  (if (executable-find "mdopen")
      (setq grip-use-mdopen t)
    (when-let* ((credential (and (require 'auth-source nil t)
                                 (auth-source-user-and-password "api.github.com"))))
      (setq grip-github-user (car credential)
            grip-github-password (cadr credential)))))

(provide 'init-markdown)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-markdown.el ends here
