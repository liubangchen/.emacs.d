;; init-c.el --- Initialize c configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2025 Vincent Zhang

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
;; C/C++ configuration.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(declare-function centaur-treesit-available-p "init-funcs")

;; C/C++ Mode
(use-package cc-mode
  :init (setq-default c-basic-offset 4))

(when (centaur-treesit-available-p)
  (use-package c-ts-mode
    :init
    (setq c-ts-mode-indent-offset 4)

    (when (boundp 'major-mode-remap-alist)
      (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
      (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
      (add-to-list 'major-mode-remap-alist
                       '(c-or-c++-mode . c-or-c++-ts-mode)))))

;;(use-package eglot-inactive-regions
;;  :custom
;;  (eglot-inactive-regions-style 'shade-background)
;;  (eglot-inactive-regions-opacity 0.4)
;;  :config
;;  (eglot-inactive-regions-mode 1))
;;
;;(use-package format-all
;;  :ensure t
;;  :hook
;;  (prog-mode . format-all-ensure-formatter)
;;  :config
;;  ;; Enable format-all-mode for programming modes (optional)
;;  (add-hook 'prog-mode-hook 'format-all-mode)
;;
;;  ;; Bind key for formatting the current buffer
;;  (define-key prog-mode-map (kbd "C-c f") 'format-all-buffer)
;;
;;  ;; Optionally, format on save
;;  (add-hook 'prog-mode-hook
;;            (lambda ()
;;              (add-hook 'before-save-hook 'format-all-buffer nil 'local))))
                                        ;
;;(setq clang-format-style "file")

(provide 'init-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
