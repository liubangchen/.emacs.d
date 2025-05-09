;;; -*- lexical-binding: t; -*-
(setq make-backup-files nil)
(setq backup-directory-alist (quote (("." . "~/backups"))))
(global-set-key [escape] 'keyboard-escape-quit)         ;;;; everywhere else

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
  (setq ssh-deploy-line-mode t
        ssh-deploy-add-menu t
        ssh-deploy-hydra "C-c C-z"))

(setenv "TZ" "Asia/Beijing")
(customize-set-variable 'tramp-encoding-shell "/bin/zsh")
;;(setq tramp-shell-prompt-pattern       "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
(customize-set-variable
 'tramp-password-prompt-regexp
 (concat
  "^.*"
  (regexp-opt
   '("passphrase" "Passphrase"
     ;; English
     "password" "Password"
     ;; Deutsch
     "passwort" "Passwort"
     ;; Français
     "mot de passe" "Mot de passe")
   t)
  ".*:\0? *"))

(setq c-toggle-electric-state nil)
;;(progn
;;  (global-set-key [mouse-1] 'mouse-set-point)
;;  (global-unset-key [down-mouse-1])
;;  (global-unset-key [drag-mouse-1]))
(provide 'init-config)
