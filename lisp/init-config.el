

;; 语法高亮
(global-font-lock-mode t)
;; 以 y/n代表 yes/no
(fset 'yes-or-no-p 'y-or-n-p)
;; 显示括号匹配
(scroll-bar-mode 0)
(global-hl-line-mode 1)
(tool-bar-mode 0)
(show-paren-mode t)
(setq show-paren-style 'parentheses)
;;;; 显示时间，格式如下
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq inhibit-startup-message t)
(setq display-time-day-and-date t)
(transient-mark-mode t)
;; 支持emacs和外部程序的粘贴
(setq x-select-enable-clipboard t)
;; 不产生备份文件
(setq make-backup-files nil)
;;不产生临时文件
(setq-default make-backup-files nil)

(setq-default cursor-type '(bar . 1))
;;设置光标形状
(setq default-cursor-type 'bar)
(set-cursor-color "#ffffff")                    ;;设置备份文件路径
(setq backup-directory-alist (quote (("." . "~/backups"))))


(defun no-junk-please-were-unixish ()
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (setq coding-str
            (concat (substring coding-str 0 (match-beginning 0)) "-unix"))
      (message "CODING: %s" coding-str)
      (set-buffer-file-coding-system (intern coding-str)) )))
(add-hook 'find-file-hooks 'no-junk-please-were-unixish)

(define-key isearch-mode-map [escape] 'isearch-abort)   ;;;; isearch
(global-set-key [escape] 'keyboard-escape-quit)         ;;;; everywhere else
;;设置选中区域的颜色
(set-face-attribute 'region nil :background "#99CC00" :foreground "#ffffff")
;;(setq helm-locate-command "locate %s  -d /Users/chenlong/locate.database -l 100  %s")
;;(setq yas-snippet-dirs '("~/.emacs.d/snippets/emacs-snippets"))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)



(use-package logview
  :init
  (setq logview-additional-submodes
        '(("clickhouse"
           (format . "TIMESTAMP [ THREAD ] {NAME} <LEVEL> MESSAGE")
           (levels . "clickhouse-log-level")
           ;; define timestamp if not one of standard
           ;;(timestamp . "yyyy-MM-dd HH:mm:ss.UUU")
           (aliases "clickhouse-log"))))

  (setq logview-additional-level-mappings
        '(("clickhouse-log-level"
           (error "Error" "Fatal")
           (warning "Warn")
           (information "Information")
           (debug "Debug")
           (trace "Trace"))))
  )

(use-package ssh-deploy
  :ensure t
  :demand
  :after hydra
  :hook ((after-save . ssh-deploy-after-save)
         (find-file . ssh-deploy-find-file))
  :config
  (ssh-deploy-line-mode) ;; If you want mode-line feature
  (ssh-deploy-add-menu) ;; If you want menu-bar feature
  (ssh-deploy-hydra "C-c C-z") ;; If you want the hydra feature
  )
(setenv "TZ" "Asia/Beijing")
(setq tramp-shell-prompt-pattern       "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")


(provide 'init-config)
