;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

(setq centaur-logo nil)                        ; Logo file or nil (official logo)
(setq centaur-full-name "liubangchen")           ; User full name
(setq centaur-mail-address  "liubangchen@tencent.com")   ; Email address
;; (setq centaur-proxy "127.0.0.1:1080")          ; Network proxy
;; (setq centaur-server nil)                      ; Enable `server-mode' or not: t or nil
(setq centaur-icon t)                        ; Display icons or not: t or nil
(setq centaur-package-archives 'melpa)         ; Package repo: melpa, emacs-china, netease, ustc, tencent or tuna
(setq centaur-theme 'doom-xcode)                     ; Color theme: auto, random, system, default, pro, dark, light, warm, cold, day or night doom-solarized-dark
(setq centaur-completion-style 'minibuffer)    ; Completion display style: minibuffer or childframe
(setq centaur-org-directory "~/notes/org")
;; (setq centaur-dashboard nil)                   ; Use dashboard at startup or not: t or nil
(setq centaur-restore-frame-geometry t)      ; Restore the frame's geometry at startup: t or nil
(setq centaur-lsp 'lsp-mode)                      ; Set LSP client: lsp-mode, eglot or nil
;; (setq centaur-lsp-format-on-save-ignore-modes '(c-mode c++-mode)) ; Ignore format on save for some languages
(setq centaur-chinese-calendar t)              ; Use Chinese calendar or not: t or nil
(setq package-check-signature nil)
;; (setq centaur-prettify-symbols-alist nil)      ; Alist of symbol prettifications. Nil to use font supports ligatures.
;; (setq centaur-prettify-org-symbols-alist nil)  ; Alist of symbol prettifications for `org-mode'

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

;; Fonts
;;(when (display-graphic-p)
;;  ;; Set default font
;;  (cl-loop for font in '("Sarasa Mono SC" "SF Mono" "Hack" "Source Code Pro" "Fira Code"
;;                         "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
;;           when (font-installed-p font)
;;           return (set-face-attribute 'default nil
;;                                      :font font
;;                                      :height (cond (sys/mac-x-p 130)
;;                                                    (sys/win32p 110)
;;                                                    (t 100))))
;;
;;  ;; Specify font for all unicode characters
;;  (cl-loop for font in '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
;;           when (font-installed-p font)
;;           return(set-fontset-font t 'unicode font nil 'prepend))
;;
;;  ;; Specify font for Chinese characters
;;  (cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
;;           when (font-installed-p font)
;;           return (set-fontset-font t '(#x4e00 . #x9fff) font)))
;;
;;
(defun set-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format   "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))

(set-font   "Monaco" "Hiragino Sans GB" 14 16)
;;(if window-system
;;    (set-font   "Sarasa Mono SC" "Sarasa Mono SC" 15 15))
;;(set-font   "Sarasa Mono SC" "Sarasa Mono SC" 16 16)

;; Mail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587
;;                                    user-mail-address nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; Calendar
;; Set location , then press `S' can show the time of sunrise and sunset
;; (setq calendar-location-name "Chengdu"
;;       calendar-latitude 30.67
;;       calendar-longitude 104.07)

;; Misc.
;; (setq confirm-kill-emacs 'y-or-n-p)

;; Enable proxy
;; (proxy-http-enable)

;; Display on the specified monitor
;; (when (and (> (length (display-monitor-attributes-list)) 1)
;;            (> (display-pixel-width) 1920))
;;   (set-frame-parameter nil 'left 1920))
;;; custom.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(centaur-package-archives 'melpa)
 '(centaur-theme 'dark)
 '(custom-safe-themes
   '("6c531d6c3dbc344045af7829a3a20a09929e6c41d7a7278963f7d3215139f6a7" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "745d03d647c4b118f671c49214420639cb3af7152e81f132478ed1c649d4597d" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" default))
 '(org-display-custom-times t)
 '(org-refile-targets
   '(("inbox.org" :level . 1)
     ("doing.org" :level . 1)
     ("personal.org" :level . 1)
     ("finished.org" :level . 1)))
 '(org-time-stamp-custom-formats '("<%Y-%m-%d %A>" . "<%Y %m %d  %A [%H:%M]>")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 2.0))))
 '(aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
 '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
 '(dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
 '(diff-hl-change ((t (:foreground "#237AD3" :background nil))))
 '(diff-hl-delete ((t (:inherit diff-removed :background nil))))
 '(diff-hl-insert ((t (:inherit diff-added :background nil))))
 '(doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
 '(git-timemachine-minibuffer-author-face ((t (:inherit success))))
 '(git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
 '(ivy-minibuffer-match-face-1 ((t (:foreground "#777778"))))
 '(lsp-headerline-breadcrumb-path-error-face ((t :underline (:style wave :color "#D16969") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-hint-face ((t :underline (:style wave :color "#579C4C") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-info-face ((t :underline (:style wave :color "#579C4C") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-warning-face ((t :underline (:style wave :color "#D7BA7D") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-symbols-error-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#D16969"))))
 '(lsp-headerline-breadcrumb-symbols-hint-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#579C4C"))))
 '(lsp-headerline-breadcrumb-symbols-info-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#579C4C"))))
 '(lsp-headerline-breadcrumb-symbols-warning-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#D7BA7D"))))
 '(lsp-ui-sideline-code-action ((t (:inherit warning))))
 '(macrostep-expansion-highlight-face ((t (:inherit tooltip :extend t))))
 '(org-ellipsis ((t (:foreground nil))))
 '(org-pomodoro-mode-line ((t (:inherit warning))))
 '(org-pomodoro-mode-line-break ((t (:inherit success))))
 '(org-pomodoro-mode-line-overtime ((t (:inherit error))))
 '(pulse-highlight-face ((t (:inherit region))))
 '(pulse-highlight-start-face ((t (:inherit region))))
 '(symbol-overlay-default-face ((t (:inherit (region bold)))))
 '(ztreep-arrow-face ((t (:inherit font-lock-comment-face))))
 '(ztreep-diff-header-face ((t (:inherit (diff-header bold)))))
 '(ztreep-diff-header-small-face ((t (:inherit diff-file-header))))
 '(ztreep-diff-model-add-face ((t (:inherit diff-nonexistent))))
 '(ztreep-diff-model-diff-face ((t (:inherit diff-removed))))
 '(ztreep-diff-model-ignored-face ((t (:inherit font-lock-doc-face :strike-through t))))
 '(ztreep-diff-model-normal-face ((t (:inherit font-lock-doc-face))))
 '(ztreep-expand-sign-face ((t (:inherit font-lock-function-name-face))))
 '(ztreep-header-face ((t (:inherit diff-header))))
 '(ztreep-leaf-face ((t (:inherit diff-index))))
 '(ztreep-node-face ((t (:inherit font-lock-variable-name-face)))))
