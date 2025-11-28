;; init-ai   --- Better default configurations.	-*- lexical-binding: t -*-
;;
;;
;;; code

(use-package minuet
  :init
  ;; 如需启用自动补全
  ;; 注意：即使不启用 minuet-auto-suggestion-mode，也可以手动触发补全
  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
  :bind
  ;; use completion-in-region for completion
  ("M-y" . minuet-completion-region)
  ("M-p" . minuet-previous-suggestion) ;; invoke completion or cycle to next completion
  ("M-n" . minuet-next-suggestion) ;; invoke completion or cycle to previous completion
  ("M-A" . minuet-accept-suggestion) ;; accept whole completion
  ("M-a" . minuet-accept-suggestion-line) ;; accept current line completion
  ("M-e" . minuet-dismiss-suggestion)

  :config
  (setq minuet-provider 'openai-fim-compatible)
  (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:11434/v1/completions")
  ;; an arbitrary non-null environment variable as placeholder
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
  (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:32b")
  )


(setq python-interpreter "~/.pyenv/shims/python3")
(setq python-shell-interpreter "~/.pyenv/shims/python3")

(use-package vterm
  :ensure t)

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setq aidermacs-auto-commits nil
        aidermacs-exit-kills-buffer t
        aidermacs-extra-args '("--thinking-tokens" "128k")
        aidermacs-backend 'vterm))
;;(setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434")
;;:custom
;;(aidermacs-default-chat-mode 'architect)
;;(aidermacs-default-model "ollama_chat/qwen2.5-coder:14b"))

(use-package ellama
  :ensure t
  :init
  ;; 设定默认使用的本地模型 (确保你已经在终端运行过 `ollama pull zephyr`)
  (setopt ellama-language "Chinese") ;; 让它尽量用中文回答
  (require 'llm-ollama)
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "qwen3-coder:30b" :embedding-model "pull granite-embedding:latest")))

(provide 'init-ml)
