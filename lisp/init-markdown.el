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
  (expand-file-name "preview.html" my/markdown-cache-dir)
  "pandoc 自定义 HTML 模板路径。")

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
                " --syntax-highlighting=pygments"
                " --embed-resources"
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
                  (my/markdown-preview-setup-keymap buf))
                (or buf (current-buffer)))))))

  (defun my/markdown-preview-jump-back ()
    "从 xwidget 预览窗口跳回 markdown 源 buffer。"
    (interactive)
    (if-let* ((src markdown-live-preview-source-buffer)
              (win (get-buffer-window src)))
        (select-window win)
      ;; 退而求其次：跳到上一个窗口
      (other-window -1)))

  (defun my/markdown-preview-setup-keymap (preview-buf)
    "为 Markdown 预览的 xwidget-webkit buffer 设置精简按键，
避免方向键/鼠标滚轮被劫持导致无法操作其他窗口。"
    (with-current-buffer preview-buf
      ;; 创建一个简化版 keymap：保留 xwidget 基本功能，
      ;; 但把方向键/常用导航键改为跳回源 buffer
      (let ((map (make-sparse-keymap)))
        ;; 鼠标滚轮：在预览区域仍可滚动预览内容
        (define-key map [mouse-4] #'xwidget-webkit-scroll-down-line)
        (define-key map [mouse-5] #'xwidget-webkit-scroll-up-line)
        (define-key map [wheel-up] #'xwidget-webkit-scroll-down)
        (define-key map [wheel-down] #'xwidget-webkit-scroll-up)
        ;; 方向键：跳回 markdown 编辑窗口，不再劫持
        (define-key map [up]    #'my/markdown-preview-jump-back)
        (define-key map [down]  #'my/markdown-preview-jump-back)
        (define-key map [left]  #'my/markdown-preview-jump-back)
        (define-key map [right] #'my/markdown-preview-jump-back)
        ;; q / ESC 跳回
        (define-key map "q" #'my/markdown-preview-jump-back)
        (define-key map (kbd "ESC") #'my/markdown-preview-jump-back)
        ;; 保留一些 xwidget 有用的按键
        (define-key map "+" #'xwidget-webkit-zoom-in)
        (define-key map "-" #'xwidget-webkit-zoom-out)
        (define-key map "r" #'xwidget-webkit-reload)
        (define-key map "0" #'xwidget-webkit-zoom-in)  ; 重置缩放
        ;; 使用 buffer-local minor mode keymap 覆盖 xwidget-webkit-mode-map
        (setq-local my/markdown-preview-mode-map map)
        (use-local-map (make-composed-keymap map xwidget-webkit-mode-map)))))

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
