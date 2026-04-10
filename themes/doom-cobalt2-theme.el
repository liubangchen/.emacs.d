;;; doom-cobalt2-theme.el --- inspired by Wes Bos's Cobalt2 -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: chenlong
;; Source: https://github.com/wesbos/cobalt2-vscode
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;; Compiler pacifier
(defvar modeline-bg)


;;
;;; Variables

(defgroup doom-cobalt2-theme nil
  "Options for the `doom-cobalt2' theme."
  :group 'doom-themes)

(defcustom doom-cobalt2-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-cobalt2-theme
  :type 'boolean)

(defcustom doom-cobalt2-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-cobalt2-theme
  :type 'boolean)

(defcustom doom-cobalt2-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-cobalt2-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-cobalt2
  "A dark theme inspired by Wes Bos's Cobalt2"

  ;; name        default   256       16
  ((bg         '("#182637" "#182637" nil          ))
   (bg-alt     '("#111C28" "#111c28" nil          ))
   (base0      '("#0E1820" "#0e1820" "black"      ))
   (base1      '("#152433" "#152433" "brightblack"))
   (base2      '("#1E3245" "#1e3245" "brightblack"))
   (base3      '("#274058" "#274058" "brightblack"))
   (base4      '("#3D5A6F" "#3d5a6f" "brightblack"))
   (base5      '("#5E7D8F" "#5e7d8f" "brightblack"))
   (base6      '("#7E9AAB" "#7e9aab" "brightblack"))
   (base7      '("#A0B8C8" "#a0b8c8" "brightblack"))
   (base8      '("#D9E6F2" "#d9e6f2" "white"      ))
   (fg         '("#D9E6F2" "#d9e6f2" "brightwhite"))
   (fg-alt     '("#A0B8C8" "#a0b8c8" "white"      ))

   (grey       base5)
   (red        '("#EA3323" "#ea3323" "red"          ))
   (orange     '("#FF9D00" "#ff9d00" "brightred"    ))
   (green      '("#70DB49" "#70db49" "green"        ))
   (teal       '("#2AFFDF" "#2affdf" "brightgreen"  ))
   (yellow     '("#FFC600" "#ffc600" "yellow"       ))
   (blue       '("#57C7FF" "#57c7ff" "brightblue"   ))
   (dark-blue  '("#0050A4" "#0050a4" "blue"         ))
   (magenta    '("#FF68B8" "#ff68b8" "magenta"      ))
   (violet     '("#FB94EE" "#fb94ee" "brightmagenta"))
   (cyan       '("#9EFFFF" "#9effff" "brightcyan"   ))
   (dark-cyan  '("#2AFFDF" "#2affdf" "cyan"         ))

   ;; face categories -- required for all themes
   ;; Code coloring inspired by Xcode Dark theme
   (highlight      yellow)
   (vertical-bar   (doom-darken base2 0.2))
   (selection      dark-blue)
   (builtin        teal)
   (comments       (if doom-cobalt2-brighter-comments base6 base5))
   (doc-comments   (doom-lighten (if doom-cobalt2-brighter-comments base6 base5) 0.25))
   (constants      violet)
   (functions      violet)
   (keywords       magenta)
   (methods        blue)
   (operators      orange)
   (type           blue)
   (strings        red)
   (variables      blue)
   (numbers        yellow)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base0) 0.35)))
   (error          red)
   (warning        orange)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (-modeline-bright doom-cobalt2-brighter-modeline)
   (-modeline-pad
    (when doom-cobalt2-padded-modeline
      (if (integerp doom-cobalt2-padded-modeline) doom-cobalt2-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt (doom-blend violet base4 (if -modeline-bright 0.5 0.2)))

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken bg 0.475)
      `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-darken (car bg) 0.1) ,@(cdr base1)))
   (modeline-bg-inactive-l (doom-darken bg 0.1)))


  ;;;; Base theme face overrides
  (((cursor &override) :background "#F4D300")
   ((font-lock-comment-face &override)
    :foreground comments
    :background (if doom-cobalt2-brighter-comments (doom-lighten bg 0.05) 'unspecified))
   ((font-lock-doc-face &override) :foreground doc-comments)
   ((font-lock-keyword-face &override) :weight 'bold)
   ((line-number &override) :foreground base5)
   ((line-number-current-line &override) :foreground yellow)
   (hl-line :background base2)
   (fringe :background bg :foreground base5)

   ;;;; mode-line
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; tooltip / popup / flycheck / flymake — 防止背景发白
   (tooltip :background (doom-darken bg 0.2) :foreground fg)
   ((secondary-selection &override) :background base2)
   (flycheck-posframe-background-face :background (doom-darken bg 0.2))
   (flycheck-posframe-border-face :foreground base3)
   (flycheck-posframe-info-face :foreground cyan)
   (flycheck-posframe-warning-face :foreground orange)
   (flycheck-posframe-error-face :foreground red)
   (flymake-error :underline `(:style wave :color ,red))
   (flymake-warning :underline `(:style wave :color ,orange))
   (flymake-note :underline `(:style wave :color ,green))

   ;;;; compilation / grep
   (compilation-info :foreground green)
   (compilation-warning :foreground orange)
   (compilation-error :foreground red)
   (compilation-line-number :foreground cyan)
   (compilation-column-number :foreground violet)

   ;;;; diff / ediff / magit
   (diff-added :foreground green :background (doom-blend green bg 0.1))
   (diff-removed :foreground red :background (doom-blend red bg 0.1))
   (diff-changed :foreground orange :background (doom-blend orange bg 0.1))
   (diff-header :foreground blue :background (doom-darken bg 0.1))
   (diff-file-header :foreground yellow :weight 'bold :background (doom-darken bg 0.1))

   ;;;; tree-sitter
   (tree-sitter-hl-face:function :foreground violet)
   (tree-sitter-hl-face:function.call :foreground violet)
   (tree-sitter-hl-face:method :foreground blue)
   (tree-sitter-hl-face:method.call :foreground blue)
   (tree-sitter-hl-face:keyword :foreground magenta :weight 'bold)
   (tree-sitter-hl-face:string :foreground red)
   (tree-sitter-hl-face:type :foreground blue)
   (tree-sitter-hl-face:variable :foreground blue)
   (tree-sitter-hl-face:variable.builtin :foreground teal)
   (tree-sitter-hl-face:constant :foreground violet)
   (tree-sitter-hl-face:constant.builtin :foreground violet)
   (tree-sitter-hl-face:number :foreground yellow)
   (tree-sitter-hl-face:operator :foreground orange)
   (tree-sitter-hl-face:property :foreground cyan)
   (tree-sitter-hl-face:comment :foreground comments)
   (tree-sitter-hl-face:doc :foreground doc-comments)
   (tree-sitter-hl-face:tag :foreground cyan)
   (tree-sitter-hl-face:attribute :foreground orange)
   (tree-sitter-hl-face:punctuation :foreground base7)
   (tree-sitter-hl-face:punctuation.bracket :foreground base7)

   ;;;; lsp / eglot
   (lsp-face-highlight-read :background base2)
   (lsp-face-highlight-write :background base2)
   (lsp-face-highlight-textual :background base2)
   (lsp-headerline-breadcrumb-path-face :foreground base7)
   (lsp-headerline-breadcrumb-separator-face :foreground base5)
   (eglot-highlight-symbol-face :background base2)

   ;;;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground cyan)
   (css-selector             :foreground teal)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground yellow)
   (markdown-header-face-1 :inherit 'bold :foreground yellow)
   (markdown-header-face-2 :inherit 'bold :foreground blue)
   (markdown-header-face-3 :inherit 'bold :foreground violet)
   (markdown-header-face-4 :inherit 'bold :foreground orange)
   (markdown-header-face-5 :inherit 'bold :foreground magenta)
   (markdown-header-face-6 :inherit 'bold :foreground cyan)
   ((markdown-code-face &override) :background (doom-darken bg 0.15) :foreground fg)
   (markdown-pre-face :foreground fg :background (doom-darken bg 0.15))
   (markdown-inline-code-face :foreground teal :background (doom-darken bg 0.15) :weight 'semi-bold)
   (markdown-table-face :foreground base7 :background bg)
   (markdown-language-keyword-face :foreground orange :weight 'semi-bold)
   (markdown-bold-face :foreground orange :weight 'bold)
   (markdown-italic-face :foreground violet :slant 'italic)
   (markdown-link-face :foreground blue :underline t)
   (markdown-url-face :foreground dark-cyan :underline t)
   (markdown-reference-face :foreground violet)
   (markdown-blockquote-face :foreground teal :slant 'italic)
   (markdown-list-face :foreground cyan)
   (markdown-footnote-marker-face :foreground teal)
   (markdown-footnote-text-face :foreground base7)
   (markdown-hr-face :foreground base4 :strike-through t)
   (markdown-highlight-face :background (doom-blend yellow bg 0.15) :foreground yellow)
   (markdown-line-break-face :inherit 'font-lock-constant-face :underline t)
   (markdown-html-tag-name-face :foreground cyan)
   (markdown-html-attr-name-face :foreground orange)
   (markdown-html-attr-value-face :foreground yellow)
   (markdown-metadata-key-face :foreground teal)
   (markdown-metadata-value-face :foreground orange)

   ;;;; org <built-in>
   (org-hide :foreground bg)
   (org-document-title :foreground yellow :weight 'bold)
   (org-document-info :foreground blue)
   (org-document-info-keyword :foreground base5)
   (org-level-1 :foreground yellow :weight 'bold)
   (org-level-2 :foreground blue :weight 'bold)
   (org-level-3 :foreground violet :weight 'bold)
   (org-level-4 :foreground orange :weight 'bold)
   (org-level-5 :foreground magenta :weight 'bold)
   (org-level-6 :foreground cyan :weight 'bold)
   (org-level-7 :foreground teal :weight 'bold)
   (org-level-8 :foreground green :weight 'bold)
   (org-todo :foreground red :weight 'bold)
   (org-done :foreground green :weight 'bold :strike-through t)
   (org-headline-done :foreground base5 :strike-through t)
   (org-date :foreground blue :underline t)
   (org-tag :foreground orange :weight 'semi-bold)
   (org-priority :foreground red :weight 'bold)
   (org-special-keyword :foreground base5)
   (org-meta-line :foreground base5)
   (org-link :foreground blue :underline t)
   (org-footnote :foreground teal :underline t)
   (org-verbatim :foreground teal :background (doom-darken bg 0.15) :weight 'semi-bold)
   (org-code :foreground cyan :background (doom-darken bg 0.15) :weight 'semi-bold)
   (org-table :foreground base7)
   (org-formula :foreground orange)
   (org-quote :foreground teal :slant 'italic)
   (org-verse :foreground violet :slant 'italic)
   (org-checkbox :foreground green :weight 'bold)
   (org-checkbox-statistics-todo :foreground red)
   (org-checkbox-statistics-done :foreground green)
   (org-list-dt :foreground cyan :weight 'bold)
   (org-drawer :foreground base5)
   (org-ellipsis :foreground base5)
   (org-agenda-date :foreground blue)
   (org-agenda-date-today :foreground yellow :weight 'bold)
   (org-agenda-date-weekend :foreground orange)
   (org-agenda-structure :foreground violet)
   (org-scheduled :foreground green)
   (org-scheduled-today :foreground green :weight 'bold)
   (org-scheduled-previously :foreground orange)
   (org-upcoming-deadline :foreground red)
   (org-warning :foreground orange :weight 'bold)
   ((org-block &override) :background (doom-darken bg-alt 0.15) :foreground 'unspecified :extend t)
   ((org-block-begin-line &override) :background (doom-darken bg-alt 0.15) :foreground teal :weight 'semi-bold)
   ((org-block-end-line &override) :background (doom-darken bg-alt 0.15) :foreground base5)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground yellow)
   (rainbow-delimiters-depth-2-face :foreground blue)
   (rainbow-delimiters-depth-3-face :foreground magenta)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground orange)
   (rainbow-delimiters-depth-6-face :foreground violet)
   (rainbow-delimiters-depth-7-face :foreground green)
   ;;;; rjsx-mode
   (rjsx-tag :foreground cyan)
   (rjsx-attr :foreground orange)

   ;;;; scala-mode
   (scala-font-lock:var-keyword-face :foreground magenta :weight 'bold)
   (scala-font-lock:val-keyword-face :foreground magenta :weight 'bold)
   (scala-font-lock:def-keyword-face :foreground magenta :weight 'bold)
   (scala-font-lock:keyword-face :foreground magenta :weight 'bold)
   (scala-font-lock:type-face :foreground blue)
   (scala-font-lock:string-face :foreground red)
   (scala-font-lock:operator-face :foreground orange)
   (scala-font-lock:var-face :foreground blue)
   (scala-font-lock:sealed-face :foreground magenta :weight 'bold)
   (scala-font-lock:implicit-face :foreground teal)

   ;;;; font-lock (all languages) — xcode-inspired
   ((font-lock-function-name-face &override) :foreground violet)
   ((font-lock-function-call-face &override) :foreground violet)
   ((font-lock-variable-name-face &override) :foreground blue)
   ((font-lock-type-face &override) :foreground blue)
   ((font-lock-string-face &override) :foreground red)
   ((font-lock-constant-face &override) :foreground violet)
   ((font-lock-builtin-face &override) :foreground teal)
   ((font-lock-keyword-face &override) :foreground magenta :weight 'bold)
   ((font-lock-number-face &override) :foreground yellow)
   ((font-lock-operator-face &override) :foreground orange)
   ((font-lock-property-name-face &override) :foreground cyan)
   ((font-lock-property-use-face &override) :foreground cyan)
   ((font-lock-preprocessor-face &override) :foreground orange)
   ((font-lock-negation-char-face &override) :foreground orange)
   ((font-lock-regexp-grouping-backslash &override) :foreground orange)
   ((font-lock-regexp-grouping-construct &override) :foreground violet)

   ;;;; go-mode
   (go-mode-function-name-face :foreground violet)

   ;;;; rust-mode
   (rust-builtin-formatting-macro :foreground teal)
   (rust-question-mark :foreground orange)
   (rust-string-interpolation :foreground red)

   ;;;; python-mode
   (python-object-reference-face :foreground teal)

   ;;;; java / kotlin
   (java-function-name-face :foreground violet)

   ;;;; web-mode
   (web-mode-html-tag-face :foreground cyan)
   (web-mode-html-tag-bracket-face :foreground base7)
   (web-mode-html-attr-name-face :foreground orange)
   (web-mode-html-attr-value-face :foreground red)
   (web-mode-html-attr-equal-face :foreground orange)
   (web-mode-function-call-face :foreground violet)
   (web-mode-function-name-face :foreground violet)
   (web-mode-string-face :foreground red)
   (web-mode-keyword-face :foreground magenta :weight 'bold)
   (web-mode-type-face :foreground blue)
   (web-mode-variable-name-face :foreground blue)
   (web-mode-constant-face :foreground violet)
   (web-mode-builtin-face :foreground teal)
   (web-mode-comment-face :foreground comments)
   (web-mode-css-selector-face :foreground cyan)
   (web-mode-css-property-name-face :foreground green)
   (web-mode-css-color-face :foreground orange)
   (web-mode-css-at-rule-face :foreground teal)
   (web-mode-css-pseudo-class-face :foreground orange)
   (web-mode-json-key-face :foreground cyan)
   (web-mode-json-context-face :foreground blue)

   ;;;; js2-mode / js-mode
   (js2-function-call :foreground violet)
   (js2-function-param :foreground blue)
   (js2-object-property :foreground cyan)
   (js2-jsdoc-tag :foreground orange)
   (js2-jsdoc-type :foreground blue)
   (js2-jsdoc-value :foreground teal)
   (js2-external-variable :foreground orange)
   (js2-instance-member :foreground cyan)
   (js2-private-function-call :foreground violet)

   ;;;; typescript-mode
   (typescript-jsdoc-tag :foreground teal)
   (typescript-jsdoc-type :foreground cyan)
   (typescript-jsdoc-value :foreground violet)

   ;;;; sql-mode
   (font-lock-sql-keyword-face :foreground magenta :weight 'bold)

   ;;;; sh-mode / bash
   (sh-heredoc :foreground red)
   (sh-quoted-exec :foreground orange)

   ;;;; nxml-mode
   (nxml-element-local-name :foreground cyan)
   (nxml-attribute-local-name :foreground orange)
   (nxml-attribute-value :foreground yellow)
   (nxml-tag-delimiter :foreground base7)
   (nxml-processing-instruction-target :foreground teal)

   ;;;; yaml-mode
   (font-lock-variable-name-face :foreground blue)
   ;;;; show-paren
   ((show-paren-match &override) :foreground yellow :background base3 :weight 'ultra-bold)
   ((show-paren-mismatch &override) :foreground bg :background red)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))))

;;; doom-cobalt2-theme.el ends here
