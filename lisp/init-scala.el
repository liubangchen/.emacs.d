;; doc https://scalameta.org/metals/docs/editors/emacs.html
;;
;; Enable scala-mode and sbt-mode
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook (scala-mode . lsp)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-metals)

;;(use-package lsp-ui)

;; Add company-lsp backend for metals
;;(use-package yasnippet)

;; Add company-lsp backend for metals
;;(use-package company-lsp)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  )

;; Use the Tree View Protocol for viewing the project structure and triggering compilation
;(use-package lsp-treemacs
;  :config
  ;;(lsp-metals-treeview-enable t)
  ;;(setq lsp-metals-treeview-show-when-views-received t)
;;  )

;;(use-package groovy-mode
;;  :init
;;  (setq groovy-indent-offset 2)
;;  :mode (("\\.groovy$" . groovy-mode)
;;         ("\\.gradle$" . groovy-mode)))
;;
;;(require 'lsp-groovy)
;;(setq lsp-groovy-server-file "~/.emacs.d/lib-lisp/javalibs/groovy-language-server-all.jar")

;;(use-package lsp-groovy
;;  :config
;;  (add-hook 'groovy-mode-hook #'lsp-groovy-enable)
;;  (setq lsp-groovy-server-file "~/.emacs.d/lib-lisp/javalibs/groovy-language-server-all.jar"))


(provide 'init-scala)
