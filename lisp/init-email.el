;; 说明：
;; git clone https://github.com/djcb/mu.git
;; mu init --maildir=~/.mail --my-address=liubangchen@tencent.com
(add-to-list 'load-path "~/.emacs.d/mu/mu4e")

(use-package org-mime)
(require 'mu4e)
(require 'mu4e-contrib)
(require 'mu4e-compose)
(require 'mu4e-org)
(require 'mu4e-icalendar)
(mu4e-icalendar-setup)
(use-package mu4e-alert
  :after mu4e
  :hook ((after-init . mu4e-alert-enable-mode-line-display)
         (after-init . mu4e-alert-enable-notifications))
  :config (mu4e-alert-set-default-style 'libnotify))

(add-hook 'mu4e-view-mode-hook
          (lambda()
            ;; try to emulate some of the eww key-bindings
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))

;;(defun htmlize-and-send ()
;;  "When in an org-mu4e-compose-org-mode message, htmlize and send it."
;;  (interactive)
;;  (when (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
;;    (org-mime-htmlize)
;;    (mu4e-compose-mode)
;;    (message-send-and-exit)))

;; This overloads the amazing C-c C-c commands in org-mode with one more function
;; namely the htmlize-and-send, above.
;;(add-hook 'org-ctrl-c-ctrl-c-hook 'htmlize-and-send t)

(add-hook 'message-mode-hook 'turn-on-orgtbl)
;;(add-hook 'message-mode-hook 'turn-on-orgstruct++)
(setq mu4e-compose-format-flowed t)
(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
;;(setq message-citation-line-format "%N @ %Y-%m-%d %H:%M %Z:\n")
;;(setq message-citation-line-function 'message-insert-formatted-citation-line)
;;(setq message-kill-buffer-on-exit t)
(setq mu4e-index-cleanup nil )     ;; don't do a full cleanup check
(setq mu4e-index-lazy-check t)    ;; don't consider up-to-date dirs
(setq mu4e-use-fancy-chars t)

(setq mu4e-headers-fields
      '( (:date .  18)
         (:flags .   5)
         (:maildir .   26)
         (:from-or-to .  25)
         (:subject .  nil)))

(setq mu4e-attachment-dir  "~/Downloads/mail")
(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-maildir-list "~/.mail")
(setq mu4e-get-mail-command "offlineimap -u quiet")
(setq mu4e-update-interval 120)
(setq mu4e-sent-folder   "/outlook/Sent Items")
(setq mu4e-drafts-folder "/outlook/Drafts")
(setq mu4e-trash-folder  "/outlook/Deleted Items")
(setq mu4e-refile-folder  "/outlook/Archive")

(setq mail-user-agent 'mu4e-user-agent
      user-full-name "liubangchen"
      message-send-mail-function 'smtpmail-send-it
      send-mail-function 'smtpmail-send-it)

(setq message-send-mail-function 'smtpmail-send-it
      ;;smtpmail-stream-type 'starttls
      smtpmail-stream-type 'plain
      ;;smtpmail-starttls-credentials
      ;;'(("smtp.tencent.com" 25 nil nil))
      smtpmail-default-smtp-server "127.0.0.1"
      smtpmail-smtp-server "127.0.0.1"
      smtpmail-smtp-service 1025
      smtpmail-debug-info t)

(setq mu4e-maildir-shortcuts
      '(("/outlook/INBOX" . ?i)
        ("/outlook/INBOX.HR" . ?h)
        ("/outlook/INBOX.OA-ADMIN(OA管理员)" . ?o)
        ("/outlook/INBOX.领导" . ?l)
        ("/outlook/INBOX.重要邮件" . ?L)
        ("/outlook/Sent Items" . ?s)
        ("/outlook/Deleted Items" . ?D)
        ("/outlook/Calendar" . ?c)
        ("/outlook/Drafts" . ?d)))


(setq mu4e-compose-signature-auto-include t
      mu4e-compose-signature "
腾讯云弹性MapReduce项目组 :  liubangchen
Email :  liubangchen@tencent.com
Tel :  8513452
网址 :  https://cloud.tencent.com/product/emr ")

(provide 'init-email)
