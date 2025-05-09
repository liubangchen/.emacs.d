;;; -*- lexical-binding: t; -*-
(require 'taskjuggler-mode)
(require 'ox-taskjuggler)
(setq org-taskjuggler-default-reports '("include \"/Users/chenlong/.emacs.d/lisp/reports.tji\""))

(add-hook 'org-mode-hook (lambda()
                           (require 'ox-taskjuggler)
                           ))

(provide 'init-taskjuggle)
