;; init-ai.el --- Initialize AI configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2026 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; AI configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; Interact with ChatGPT or other LLMs
(use-package gptel
  :functions gptel-make-openai
  :custom
  (gptel-model "deepseek-chat")  ; 默认使用 DeepSeek Chat 模型
  ;; Put the apikey to `auth-sources'
  ;; Format: "machine {HOST} login {USER} password {APIKEY}"
  ;; The LLM host is used as HOST, and "apikey" as USER.
  ;; 配置 DeepSeek 后端
  (gptel-backend
   (gptel-make-openai "DeepSeek"
     :host "api.deepseek.com"
     :endpoint "/chat/completions"
     :stream t
     :key 'gptel-api-key
     :models '("deepseek-chat" "deepseek-coder")))
  :config
  ;; 设置多个后端，可以通过 C-u M-x gptel 选择
  (setq gptel-model 'deepseek-chat
        gptel-backend (gptel-get-backend "DeepSeek")))

;; Generate commit messages for magit
(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install))

;; A native shell experience to interact with ACP agents
(when emacs/>=29p
  (use-package agent-shell
    :diminish agent-shell-ui-mode))

(provide 'init-ai)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
