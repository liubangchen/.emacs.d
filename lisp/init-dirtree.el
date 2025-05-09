;;; -*- lexical-binding: t; -*-

(use-package neotree
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))
(provide 'init-dirtree)
