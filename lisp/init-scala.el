;;; -*- lexical-binding: t; -*-
;; Scala development configuration
;; Adapted from https://scalameta.org/metals/docs/editors/emacs.html
;; NOTE: flycheck/lsp-mode/posframe/yasnippet/lsp-ui are already configured
;; globally in init-check.el, init-lsp.el, init-snippet.el etc.

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :interpreter ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Add metals backend for lsp-mode
(use-package lsp-metals)

(provide 'init-scala)
