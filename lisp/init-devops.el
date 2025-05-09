;;; -*- lexical-binding: t; -*-
(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

(use-package ggtags
  :ensure t
  :commands ggtags-mode
  :diminish ggtags-mode
  :bind (("M-*" . pop-tag-mark)
         ("C-c t s" . ggtags-find-other-symbol)
         ("C-c t h" . ggtags-view-tag-history)
         ("C-c t r" . ggtags-find-reference)
         ("C-c t f" . ggtags-find-file)
         ("C-c t c" . ggtags-create-tags))
  :init
  (add-hook 'c-mode-common-hook
            #'(lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                  (ggtags-mode 1))))
  :config
  (progn
    (setq ggtags-update-on-save nil) ;Don't try to update GTAGS on each save; makes the system sluggish for huge projects.
    (setq ggtags-highlight-tag nil)  ;Don't auto-highlight tag at point.. makes the system really sluggish!
    (setq ggtags-sort-by-nearness nil) ; Enabling nearness requires global 6.5+
    (setq ggtags-navigation-mode-lighter nil)
    (setq ggtags-mode-line-project-name nil)
    (bind-key "M-." nil ggtags-mode-map)
    (bind-key "M-]" nil ggtags-mode-map)
    (bind-key "C-M-." nil ggtags-mode-map)
    (setq tags-revert-without-query t)
    (setq tags-case-fold-search nil) ; t=case-insensitive, nil=case-sensitive
    (setq ggtags-oversize-limit (* 60 1024 1024)))
  )

(use-package imenu-anywhere)

(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :hook (cmake-mode . lsp-deferred))

(use-package cmake-font-lock
  :ensure t
  :after cmake-mode
  :config (cmake-font-lock-activate))

(provide 'init-devops)
