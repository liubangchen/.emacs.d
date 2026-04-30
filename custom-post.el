;;; custom-post.el --- User post-init customizations -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Performance optimizations for large org files.
;;

;;; Code:

;; 全局关闭 org-indent-mode，依赖主题颜色和 org-modern 图标区分层次
(setq org-startup-indented nil)
(setq org-hide-leading-stars t)
(setq org-adapt-indentation nil)

;; org-mode 中禁用折行，表格列对齐显示
(add-hook 'org-mode-hook (lambda () (setq-local truncate-lines t)))

;;; custom-post.el ends here
