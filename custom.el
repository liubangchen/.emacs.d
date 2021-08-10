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
(setq centaur-theme 'dark)                     ; Color theme: auto, random, system, default, pro, dark, light, warm, cold, day or night doom-solarized-dark
(setq centaur-completion-style 'childframe)    ; Completion display style: minibuffer or childframe
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
;; (when (display-graphic-p)
;;   ;; Set default font
;;   (cl-loop for font in '("Monaco" "Sarasa Mono SC" "SF Mono" "Hack" "Source Code Pro" "Fira Code"
;;                          "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
;;            when (font-installed-p font)
;;            return (set-face-attribute 'default nil
;;                                       :font font
;;                                       :height (cond (sys/mac-x-p 130)
;;                                                     (sys/win32p 110)
;;                                                     (t 100))))

;;   ;; Specify font for all unicode characters
;;   (cl-loop for font in '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
;;            when (font-installed-p font)
;;            return(set-fontset-font t 'unicode font nil 'prepend))

;;   ;; Specify font for Chinese characters
;;   (cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
;;            when (font-installed-p font)
;;            return (set-fontset-font t '(#x4e00 . #x9fff) font)))

(defun set-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format   "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))

;;(set-font   "Monaco" "Hiragino Sans GB" 14 16)
(set-font   "Sarasa Mono SC" "Sarasa Mono SC" 18 18)

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
