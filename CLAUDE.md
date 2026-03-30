# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**Centaur Emacs** — an enhanced Emacs distribution written in Emacs Lisp. Requires Emacs 28.1+ (tested on 28.2, 29.4, 30.1, snapshot). Supports Linux, macOS, and Windows (Cygwin/MSYS2).

## Build & Test Commands

### Full configuration test (batch mode, used in CI)

```bash
cp -f custom-example.el custom.el  # required if custom.el doesn't exist
emacs -q --batch \
  --eval "(message \"Testing...\")" \
  --eval "(let ((early-init-file (locate-user-emacs-file \"early-init.el\"))
                (user-init-file (locate-user-emacs-file \"init.el\")))
            (and (>= emacs-major-version 27) (load early-init-file))
            (load user-init-file))" \
  --eval "(message \"Testing...done\")"
```

### Minimal config test (troubleshooting)

```bash
emacs -Q -l ~/.emacs.d/init-mini.el
```

### Single module test

```bash
emacs -q --batch \
  -l lisp/init-const.el \
  -l lisp/init-custom.el \
  -l lisp/init-funcs.el \
  -l lisp/init-<feature>.el \
  --eval "(message \"Module loaded\")"
```

### Byte compilation & linting (inside Emacs)

- `M-x byte-compile-file` — compile a single file
- `M-x byte-recompile-directory` — recompile `lisp/` directory
- `M-x flymake-mode` — real-time linting

## Architecture

### Entry points

- `early-init.el` — early startup optimizations (GC deferral, UI suppression, file-name-handler-alist bypass)
- `init.el` — main entry point; sets up load path (`lisp/`, `site-lisp/`) then requires ~50 init modules
- `init-mini.el` — minimal standalone config for troubleshooting

### Module loading order (from init.el)

```
Foundation:    init-const → init-custom → init-funcs → init-package → init-base → init-hydra
UI:            init-ui → init-edit → init-completion → init-snippet
Navigation:    init-bookmark → init-calendar → init-dashboard → init-dired → init-highlight
               init-ibuffer → init-kill-ring → init-workspace → init-window → init-treemacs
Shell:         init-eshell → init-shell
Docs:          init-markdown → init-org → init-reader
Utilities:     init-dict → init-docker → init-player → init-utils
Programming:   init-vcs → init-check → init-lsp → init-dap → init-ai → init-prog
Languages:     init-elisp → init-c → init-go → init-rust → init-python → init-ruby
               init-elixir → init-web → init-scala
Additional:    init-keybinds → init-config → init-devops → init-tabbar → init-latex
               init-taskjuggle → init-sql → init-plantuml → init-jupyter
               init-orgtools → init-ml → init-codebuddy
```

### Foundation modules

| Module | Purpose |
|--------|---------|
| `init-const.el` | System predicates (`sys/win32p`, `sys/macp`, `sys/linuxp`, etc.) and constants |
| `init-custom.el` | `defcustom` variables for user configuration (`centaur-theme`, `centaur-lsp`, etc.) |
| `init-funcs.el` | Shared utility functions used across modules |
| `init-package.el` | Package management setup, ELPA archive selection, `use-package` config |

### Customization system

- `custom-example.el` — template; copy to `custom.el` for user settings
- `custom.el` — user customizations (not tracked in git)
- `custom-post.el` or `custom-post.org` — loaded after init for user overrides

Key variables: `centaur-theme`, `centaur-lsp` (eglot/lsp-mode/nil), `centaur-completion-style`, `centaur-package-archives`, `centaur-icon`, `centaur-tree-sitter`

## Code Style & Conventions

### File header (required for all .el files)

```elisp
;;; filename.el --- Description -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2006-2026 Vincent Zhang
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d
```

Always use `lexical-binding: t` and `no-byte-compile: t`. End files with `(provide 'init-<feature>)`.

### Standard imports at top of module

```elisp
(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(require 'init-funcs)
```

### Naming conventions

- Files: `init-<feature>.el`
- Functions/variables: `kebab-case` (e.g., `centaur-update`, `centaur-full-name`)
- Faces: suffix with `-face`

### Package configuration pattern

Use `use-package` with lazy loading (project sets `use-package-always-ensure t` and `use-package-always-defer t`):

```elisp
(use-package package-name
  :diminish
  :hook (mode . function)
  :bind (...)
  :custom (...)
  :init (...)
  :config (...))
```

### Platform-specific code

Use predicates from `init-const.el`:

```elisp
(cond
 ((sys/win32p) ...)
 ((sys/mac-port-p) ...)
 ((sys/macp) ...)
 ((sys/linuxp) ...))
```

### Forward declarations

Use `declare-function` for external package functions:

```elisp
(declare-function consult-theme "ext:consult")
```

### Adding a new module

1. Create `lisp/init-<feature>.el` with the standard header
2. Add `(require 'init-<feature>)` in `init.el`
3. Use `use-package` for package configuration
