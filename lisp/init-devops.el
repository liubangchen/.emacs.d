;;; -*- lexical-binding: t; -*-
;; DevOps related configurations
;;
;; NOTE: The following packages have been removed as they are superseded:
;;   - ag → use rg (ripgrep) instead, configured via init-utils or consult
;;   - ggtags → use eglot/lsp-mode instead (configured in init-lsp.el)
;;   - imenu-anywhere → use consult-imenu instead
;;   - protobuf-mode → already configured in init-prog.el
;;   - cmake-mode → already configured in init-prog.el

;; cmake-font-lock still useful as an add-on to the cmake-mode in init-prog.el
(use-package cmake-font-lock
  :ensure t
  :after cmake-mode
  :config (cmake-font-lock-activate))

(provide 'init-devops)
