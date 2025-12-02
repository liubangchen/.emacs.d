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

(setenv "TZ" "Asia/Shanghai")
(setq system-time-locale "C")
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

(use-package fzf
  :bind
  ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

(use-package ls-lisp
  :ensure nil
  :hook (dired-mode . (lambda () (setq dired-use-ls-dired nil)))
  :config
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t) ;; 目录置顶
  (setq ls-lisp-use-localized-time-format t)
  (setq dired-listing-switches "-alh --time-style=long-iso")
  (setq ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M")))
;;(progn
;;  (global-set-key [mouse-1] 'mouse-set-point)
;;  (global-unset-key [down-mouse-1])
;;  (global-unset-key [drag-mouse-1]))
(provide 'init-config)
