;;; -*- lexical-binding: t; -*-
(require 'taskjuggler-mode)
(require 'ox-taskjuggler)
(setq org-taskjuggler-default-reports '("include \"/Users/chenlong/.emacs.d/lisp/reports.tji\""))

;;(setq org-taskjuggler-valid-task-attributes
;;      '(account start note duration endbuffer endcredit end
;;                flags journalentry length limits maxend maxstart minend
;;                minstart period reference responsible scheduling
;;                startbuffer startcredit statusnote chargeset charge priority))

(add-hook 'org-mode-hook (lambda()
                           (require 'ox-taskjuggler)
                           ))

(provide 'init-taskjuggle)
