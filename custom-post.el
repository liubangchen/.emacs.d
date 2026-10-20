;;; custom-post.el --- User post-init customizations -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:
;;
;; Personal AI defaults and performance optimizations for large org files.
;;

;;; Code:

;; Keep personal backend settings outside the upstream AI configuration.
(with-eval-after-load 'gptel
  (setq-default gptel-model 'deepseek-chat
                gptel-backend
                (gptel-make-openai "DeepSeek"
                  :host "api.deepseek.com"
                  :endpoint "/chat/completions"
                  :stream t
                  :key (lambda () (getenv "DEEPSEEK_API_KEY"))
                  :models '(deepseek-chat deepseek-coder))))

;; 全局关闭 org-indent-mode，依赖主题颜色和 org-modern 图标区分层次
(setq org-startup-indented nil)
(setq org-hide-leading-stars t)
(setq org-adapt-indentation nil)

;; org-mode 中禁用折行，表格列对齐显示
(add-hook 'org-mode-hook (lambda () (setq-local truncate-lines t)))

;;; custom-post.el ends here
