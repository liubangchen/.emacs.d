;;; -*- lexical-binding: t; -*-
(use-package centaur-tabs
  :demand
  :ensure t
  :config
  (centaur-tabs-mode t)
  :init
  (setq centaur-tabs-cycle-scope 'tabs
        centaur-tabs-icon-type 'nerd-icons
        centaur-tabs-style "chamfer"
        centaur-tabs-height 28
        centaur-tabs-set-icons t
        centaur-tabs-set-bar 'over
        centaur-tabs-close-button "X"
        centaur-tabs-plain-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-gray-out-icons 'buffer
        uniquify-separator "/"
        uniquify-buffer-name-style 'forward
        centaur-tabs-modified-marker "*")
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  (dired-mode . centaur-tabs-local-mode)
  :config
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :bind
  ("s-<up>" . centaur-tabs-backward)
  ("s-<down>" . centaur-tabs-forward))

(defun centaur-tabs-hide-tab (x)
  (let ((name (format "%s" x)))
    (or
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name)))
     )))
(provide 'init-tabbar)
