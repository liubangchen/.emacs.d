;;; -*- lexical-binding: t; -*-
(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in `org-mode` . "
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(set-time-zone-rule "Asia/Shanghai")

(defun org-journal-save-entry-and-exit()
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

(use-package org-journal
  :ensure t
  :defer t
  :init
  :config
  (setq org-journal-dir "~/notes/org/gtd/journal"
        org-journal-file-format "%Y-%m-%d"
        org-journal-file-type 'monthly
        org-journal-date-format "%Y-%m-%d %A"))

(setq system-time-locale "zh_CN")
(custom-set-variables
 '(org-display-custom-times t)
 '(org-time-stamp-custom-formats (quote ("[%Y-%m-%d %A]" . "[%Y %m %d  %A [%H:%M]]"))))

(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(setq org-ditaa-jar-path
      (expand-file-name "~/.emacs.d/javalibs/ditaa0_9.jar"))

(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/javalibs/plantumllib/plantuml.jar"))
(setq org-plantuml-exec-mode 'jar)
;;(setq org-plantuml-exec-mode 'plantuml)
;;(setq org-plantuml-executable-path "~/.emacs.d/javalibs/plantuml")
(setq org-plantuml-executable-args '("-headless" "-charset UTF-8"))
(setq org-confirm-babel-evaluate nil)

(use-package valign
  :hook (org-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))

;;(defun org-screenshot ()
;;  "Take a screenshot into a time stamped unique-named file in the
;;same directory as the org-buffer and insert a link to this file."
;;  (interactive)
;;  (setq filename
;;        (concat
;;         (make-temp-name
;;          (concat "images\/ss"
;;                  "_"
;;                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
;;  (call-process "screencapture" nil nil nil "-i" filename)
;;  (insert (concat "[[./" filename "]]")))

;;(setq org-babel-python-command "python3")
;; brew install pngpaste imagemagick
(defvar org-screenshot-dir "~/notes/org/notes/images/"
  "保存截图的目录，会自动创建")

(defun org-screenshot ()
  "截图→保存→插入 org 链接"
  (interactive)
  (unless (file-directory-p org-screenshot-dir)
    (make-directory org-screenshot-dir t))
  (let* ((basename (format-time-string "%Y-%m-%d_%H-%M-%S.png"))
         (path (expand-file-name basename org-screenshot-dir)))
    (cond
     ((eq system-type 'gnu/linux)
      (call-process "import" nil nil nil path)) ; ImageMagick
     ((eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" path)))
    (insert (format "[[file:%s]]" path))
    (org-display-inline-images)))

(defun org-clip-image ()
  "剪贴板有图片时保存并插入链接"
  (interactive)
  (let* ((basename (format-time-string "%Y-%m-%d_%H-%M-%S.png"))
         (path (expand-file-name basename org-screenshot-dir)))
    (unless (file-directory-p org-screenshot-dir) (make-directory org-screenshot-dir t))
    (cond
     ((eq system-type 'gnu/linux)
      (call-process "xclip" nil nil nil "-selection" "clipboard" "-t" "image/png" "-o" path))
     ((eq system-type 'darwin)
      (call-process "pngpaste" nil nil nil path))
     ((eq system-type 'windows-nt)
      (w32-read-png-from-clipboard path)))
    (insert (format "[[file:%s]]" path))
    (org-display-inline-images)))

(defun org-clip-image-current ()
  "剪贴板有图片时保存并插入链接"
  (interactive)
  (let* ((basename (format-time-string "%Y-%m-%d_%H-%M-%S.png"))
         (basedir (when buffer-file-name
                    (file-name-directory buffer-file-name)))
         (imagedir (expand-file-name "images" basedir))
         (path (expand-file-name basename imagedir)))
    (unless (file-directory-p imagedir) (make-directory imagedir t))
    (cond
     ((eq system-type 'gnu/linux)
      (call-process "xclip" nil nil nil "-selection" "clipboard" "-t" "image/png" "-o" path))
     ((eq system-type 'darwin)
      (call-process "pngpaste" nil nil nil path))
     ((eq system-type 'windows-nt)
      (w32-read-png-from-clipboard path)))
    (insert (format "[[file:%s]]" path))
    (org-display-inline-images)))


(setq org-log-into-drawer t)
(setq org-log-done 'time)
(provide 'init-orgtools)
