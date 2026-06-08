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
  ;; Palette aligned with hoblin/omarchy-cobalt2-theme
  ;; (https://github.com/hoblin/omarchy-cobalt2-theme/blob/main/colors.toml)
  ((bg         '("#122738" "#122738" nil          ))
   (bg-alt     '("#0B1D29" "#0b1d29" nil          ))
   (base0      '("#08131C" "#08131c" "black"      ))
   ;; base1/2/3 align with obsidian.css layer model:
   ;;   base1 = code-bg, base2 = card surface, base3 = hover/secondary
   (base1      '("#15232D" "#15232d" "brightblack"))
   (base2      '("#193549" "#193549" "brightblack"))
   (base3      '("#1F4662" "#1f4662" "brightblack"))
   (base4      '("#3A576D" "#3a576d" "brightblack"))
   (base5      '("#5C7B8E" "#5c7b8e" "brightblack"))
   (base6      '("#7C99AA" "#7c99aa" "brightblack"))
   (base7      '("#A0B8C8" "#a0b8c8" "brightblack"))
   (base8      '("#E6EFF7" "#e6eff7" "white"      ))
   (fg         '("#FFFFFF" "#ffffff" "brightwhite"))
   (fg-alt     '("#A0B8C8" "#a0b8c8" "white"      ))

   (grey       base5)
   (red        '("#FF628C" "#ff628c" "red"          ))
   (orange     '("#FF9D00" "#ff9d00" "brightred"    ))
   (green      '("#3AD900" "#3ad900" "green"        ))
   (teal       '("#2AFFDF" "#2affdf" "brightgreen"  ))
   (yellow     '("#FFC600" "#ffc600" "yellow"       ))
   (blue       '("#0088FF" "#0088ff" "brightblue"   ))
   (dark-blue  '("#0050A4" "#0050a4" "blue"         ))
   (magenta    '("#FF628C" "#ff628c" "magenta"      ))
   (violet     '("#FB94FF" "#fb94ff" "brightmagenta"))
   (cyan       '("#80FCFF" "#80fcff" "brightcyan"   ))
   (dark-cyan  '("#2AFFDF" "#2affdf" "cyan"         ))

   ;; face categories -- required for all themes
   ;; Code coloring follows canonical Wes Bos Cobalt2 / omarchy-cobalt2
   (highlight      yellow)
   (vertical-bar   (doom-darken base2 0.2))
   (selection      dark-blue)
   (builtin        violet)
   (comments       (if doom-cobalt2-brighter-comments base6 base5))
   (doc-comments   (doom-lighten (if doom-cobalt2-brighter-comments base6 base5) 0.25))
   (constants      violet)
   (functions      yellow)
   (keywords       orange)
   (methods        yellow)
   (operators      fg)
   (type           cyan)
   (strings        green)
   (variables      blue)
   (numbers        red)
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
   (tree-sitter-hl-face:function :foreground yellow)
   (tree-sitter-hl-face:function.call :foreground yellow)
   (tree-sitter-hl-face:method :foreground yellow)
   (tree-sitter-hl-face:method.call :foreground yellow)
   (tree-sitter-hl-face:keyword :foreground orange :weight 'bold)
   (tree-sitter-hl-face:string :foreground green)
   (tree-sitter-hl-face:type :foreground cyan)
   (tree-sitter-hl-face:variable :foreground blue)
   (tree-sitter-hl-face:variable.builtin :foreground violet)
   (tree-sitter-hl-face:constant :foreground violet)
   (tree-sitter-hl-face:constant.builtin :foreground violet)
   (tree-sitter-hl-face:number :foreground red)
   (tree-sitter-hl-face:operator :foreground fg)
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
   (markdown-header-face-5 :inherit 'bold :foreground red)
   (markdown-header-face-6 :inherit 'bold :foreground cyan)
   ((markdown-code-face &override) :background (doom-darken bg 0.15) :foreground fg)
   (markdown-pre-face :foreground fg :background (doom-darken bg 0.15))
   (markdown-inline-code-face :foreground green :background (doom-darken bg 0.15) :weight 'semi-bold)
   (markdown-table-face :foreground base7 :background bg)
   (markdown-language-keyword-face :foreground orange :weight 'semi-bold)
   (markdown-bold-face :foreground orange :weight 'bold)
   (markdown-italic-face :foreground violet :slant 'italic)
   (markdown-link-face :foreground blue :underline t)
   (markdown-url-face :foreground cyan :underline t)
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
   (markdown-html-attr-value-face :foreground green)
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
   (org-level-5 :foreground red :weight 'bold)
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
   (org-verbatim :foreground green :background (doom-darken bg 0.15) :weight 'semi-bold)
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
   (rainbow-delimiters-depth-3-face :foreground violet)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground orange)
   (rainbow-delimiters-depth-6-face :foreground red)
   (rainbow-delimiters-depth-7-face :foreground green)
   ;;;; rjsx-mode
   (rjsx-tag :foreground cyan)
   (rjsx-attr :foreground orange)

   ;;;; scala-mode
   (scala-font-lock:var-keyword-face :foreground orange :weight 'bold)
   (scala-font-lock:val-keyword-face :foreground orange :weight 'bold)
   (scala-font-lock:def-keyword-face :foreground orange :weight 'bold)
   (scala-font-lock:keyword-face :foreground orange :weight 'bold)
   (scala-font-lock:type-face :foreground cyan)
   (scala-font-lock:string-face :foreground green)
   (scala-font-lock:operator-face :foreground fg)
   (scala-font-lock:var-face :foreground blue)
   (scala-font-lock:sealed-face :foreground orange :weight 'bold)
   (scala-font-lock:implicit-face :foreground violet)

   ;;;; font-lock (all languages) — canonical Cobalt2
   ((font-lock-function-name-face &override) :foreground yellow)
   ((font-lock-function-call-face &override) :foreground yellow)
   ((font-lock-variable-name-face &override) :foreground blue)
   ((font-lock-type-face &override) :foreground cyan)
   ((font-lock-string-face &override) :foreground green)
   ((font-lock-constant-face &override) :foreground violet)
   ((font-lock-builtin-face &override) :foreground violet)
   ((font-lock-keyword-face &override) :foreground orange :weight 'bold)
   ((font-lock-number-face &override) :foreground red)
   ((font-lock-operator-face &override) :foreground fg)
   ((font-lock-property-name-face &override) :foreground cyan)
   ((font-lock-property-use-face &override) :foreground cyan)
   ((font-lock-preprocessor-face &override) :foreground orange)
   ((font-lock-negation-char-face &override) :foreground orange)
   ((font-lock-regexp-grouping-backslash &override) :foreground orange)
   ((font-lock-regexp-grouping-construct &override) :foreground violet)

   ;;;; go-mode
   (go-mode-function-name-face :foreground yellow)

   ;;;; rust-mode
   (rust-builtin-formatting-macro :foreground violet)
   (rust-question-mark :foreground orange)
   (rust-string-interpolation :foreground green)

   ;;;; python-mode
   (python-object-reference-face :foreground violet)

   ;;;; java / kotlin
   (java-function-name-face :foreground yellow)

   ;;;; web-mode
   (web-mode-html-tag-face :foreground cyan)
   (web-mode-html-tag-bracket-face :foreground base7)
   (web-mode-html-attr-name-face :foreground orange)
   (web-mode-html-attr-value-face :foreground green)
   (web-mode-html-attr-equal-face :foreground orange)
   (web-mode-function-call-face :foreground yellow)
   (web-mode-function-name-face :foreground yellow)
   (web-mode-string-face :foreground green)
   (web-mode-keyword-face :foreground orange :weight 'bold)
   (web-mode-type-face :foreground cyan)
   (web-mode-variable-name-face :foreground blue)
   (web-mode-constant-face :foreground violet)
   (web-mode-builtin-face :foreground violet)
   (web-mode-comment-face :foreground comments)
   (web-mode-css-selector-face :foreground cyan)
   (web-mode-css-property-name-face :foreground green)
   (web-mode-css-color-face :foreground orange)
   (web-mode-css-at-rule-face :foreground orange)
   (web-mode-css-pseudo-class-face :foreground orange)
   (web-mode-json-key-face :foreground cyan)
   (web-mode-json-context-face :foreground blue)

   ;;;; js2-mode / js-mode
   (js2-function-call :foreground yellow)
   (js2-function-param :foreground blue)
   (js2-object-property :foreground cyan)
   (js2-jsdoc-tag :foreground orange)
   (js2-jsdoc-type :foreground cyan)
   (js2-jsdoc-value :foreground violet)
   (js2-external-variable :foreground orange)
   (js2-instance-member :foreground cyan)
   (js2-private-function-call :foreground yellow)

   ;;;; typescript-mode
   (typescript-jsdoc-tag :foreground orange)
   (typescript-jsdoc-type :foreground cyan)
   (typescript-jsdoc-value :foreground violet)

   ;;;; sql-mode
   (font-lock-sql-keyword-face :foreground orange :weight 'bold)

   ;;;; sh-mode / bash
   (sh-heredoc :foreground green)
   (sh-quoted-exec :foreground orange)

   ;;;; nxml-mode
   (nxml-element-local-name :foreground cyan)
   (nxml-attribute-local-name :foreground orange)
   (nxml-attribute-value :foreground green)
   (nxml-tag-delimiter :foreground base7)
   (nxml-processing-instruction-target :foreground violet)

   ;;;; yaml-mode
   (font-lock-variable-name-face :foreground blue)
   ;;;; show-paren
   ((show-paren-match &override) :foreground yellow :background base3 :weight 'ultra-bold)
   ((show-paren-mismatch &override) :foreground bg :background red)

   ;;;; nerd-icons / all-the-icons — pin semantic faces to Cobalt2 palette
   ;; Both packages expose the same color names, so we map them in lockstep.
   ;; Cobalt2 has no distinct maroon/pink/silver, so we re-purpose: maroon→red,
   ;; pink→red (red IS pink in this palette), silver→fg-alt.
   (nerd-icons-red       :foreground red)
   (nerd-icons-lred      :foreground (doom-lighten red 0.3))
   (nerd-icons-dred      :foreground (doom-darken red 0.2))
   (nerd-icons-red-alt   :foreground (doom-darken red 0.1))
   (nerd-icons-green     :foreground green)
   (nerd-icons-lgreen    :foreground (doom-lighten green 0.3))
   (nerd-icons-dgreen    :foreground (doom-darken green 0.2))
   (nerd-icons-yellow    :foreground yellow)
   (nerd-icons-lyellow   :foreground (doom-lighten yellow 0.3))
   (nerd-icons-dyellow   :foreground orange)
   (nerd-icons-blue      :foreground blue)
   (nerd-icons-blue-alt  :foreground dark-blue)
   (nerd-icons-lblue     :foreground (doom-lighten blue 0.3))
   (nerd-icons-dblue     :foreground dark-blue)
   (nerd-icons-maroon    :foreground red)
   (nerd-icons-lmaroon   :foreground (doom-lighten red 0.3))
   (nerd-icons-dmaroon   :foreground (doom-darken red 0.25))
   (nerd-icons-purple    :foreground violet)
   (nerd-icons-purple-alt :foreground (doom-darken violet 0.15))
   (nerd-icons-lpurple   :foreground (doom-lighten violet 0.2))
   (nerd-icons-dpurple   :foreground (doom-darken violet 0.25))
   (nerd-icons-orange    :foreground orange)
   (nerd-icons-lorange   :foreground (doom-lighten orange 0.25))
   (nerd-icons-dorange   :foreground (doom-darken orange 0.2))
   (nerd-icons-cyan      :foreground cyan)
   (nerd-icons-cyan-alt  :foreground teal)
   (nerd-icons-lcyan     :foreground (doom-lighten cyan 0.2))
   (nerd-icons-dcyan     :foreground dark-cyan)
   (nerd-icons-pink      :foreground red)
   (nerd-icons-lpink     :foreground (doom-lighten red 0.3))
   (nerd-icons-dpink     :foreground (doom-darken red 0.2))
   (nerd-icons-silver    :foreground base7)
   (nerd-icons-lsilver   :foreground base8)
   (nerd-icons-dsilver   :foreground base5)

   (all-the-icons-red       :foreground red)
   (all-the-icons-lred      :foreground (doom-lighten red 0.3))
   (all-the-icons-dred      :foreground (doom-darken red 0.2))
   (all-the-icons-red-alt   :foreground (doom-darken red 0.1))
   (all-the-icons-green     :foreground green)
   (all-the-icons-lgreen    :foreground (doom-lighten green 0.3))
   (all-the-icons-dgreen    :foreground (doom-darken green 0.2))
   (all-the-icons-yellow    :foreground yellow)
   (all-the-icons-lyellow   :foreground (doom-lighten yellow 0.3))
   (all-the-icons-dyellow   :foreground orange)
   (all-the-icons-blue      :foreground blue)
   (all-the-icons-blue-alt  :foreground dark-blue)
   (all-the-icons-lblue     :foreground (doom-lighten blue 0.3))
   (all-the-icons-dblue     :foreground dark-blue)
   (all-the-icons-maroon    :foreground red)
   (all-the-icons-lmaroon   :foreground (doom-lighten red 0.3))
   (all-the-icons-dmaroon   :foreground (doom-darken red 0.25))
   (all-the-icons-purple    :foreground violet)
   (all-the-icons-purple-alt :foreground (doom-darken violet 0.15))
   (all-the-icons-lpurple   :foreground (doom-lighten violet 0.2))
   (all-the-icons-dpurple   :foreground (doom-darken violet 0.25))
   (all-the-icons-orange    :foreground orange)
   (all-the-icons-lorange   :foreground (doom-lighten orange 0.25))
   (all-the-icons-dorange   :foreground (doom-darken orange 0.2))
   (all-the-icons-cyan      :foreground cyan)
   (all-the-icons-cyan-alt  :foreground teal)
   (all-the-icons-lcyan     :foreground (doom-lighten cyan 0.2))
   (all-the-icons-dcyan     :foreground dark-cyan)
   (all-the-icons-pink      :foreground red)
   (all-the-icons-lpink     :foreground (doom-lighten red 0.3))
   (all-the-icons-dpink     :foreground (doom-darken red 0.2))
   (all-the-icons-silver    :foreground base7)
   (all-the-icons-lsilver   :foreground base8)
   (all-the-icons-dsilver   :foreground base5)

   ;;;; nerd-icons-dired — directory icon face defined in init-dired.el
   (nerd-icons-dired-dir-face :foreground yellow)

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
