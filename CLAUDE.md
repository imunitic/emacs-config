# Emacs Configuration

This configuration is built on top of **[emacs-bedrock](https://sr.ht/~ashton314/emacs-bedrock/)** — a minimal Emacs starter kit by Ashton314. The `early-init.el` and `init.el` originate from bedrock and retain its helpers (`bedrock--initial-gc-threshold`, `bedrock--backup-file-name`). The `lisp/` module system is the local extension layer built on top of bedrock's foundation.

## What is emacs-bedrock?

Emacs-bedrock is an intentionally minimal starter configuration for Emacs 29.1+. Its philosophy:

- **Built-in first** — leverages Emacs's native features (use-package, eglot, tree-sitter, completion) before reaching for third-party packages.
- **Educational** — every decision in the config files is explained in plain English so users understand and can adapt what they copy.
- **Not a framework** — it's a starting point, not a centrally-maintained distribution. Users are expected to own and modify the code.

Bedrock ships two core files (`early-init.el`, `init.el`) and an `extras/` directory of optional modules:

| Bedrock extra | What it covers |
|---|---|
| `extras/base.el` | Vertico, Consult, Corfu, Marginalia, Orderless, Embark |
| `extras/dev.el` | Magit, Eglot, Tree-sitter, Markdown/YAML/JSON modes |
| `extras/org.el` | Org-mode configuration |
| `extras/vim-like.el` | Evil-mode vim keybindings |
| `extras/researcher.el` | Org-roam, Citar, Denote |

In this config the `extras/` concept is replaced by the `lisp/` auto-loading module system described below. The bedrock `extras/` folder is not used directly — its ideas are absorbed into the custom modules.

---

Emacs 29+ configuration using `straight.el` for package management and `use-package` for declarative configuration. Evil (vim bindings) is the primary editing layer.

## Key Files

| File | Role |
|---|---|
| `early-init.el` | Pre-startup tweaks (GC, package.el disabled, var/ dirs) |
| `init.el` | Bootstrap straight.el, core settings, module loading |
| `custom.el` | Emacs-managed customizations (do not edit by hand) |
| `lisp/` | All custom modules (one feature per file) |

## Module System

### How modules are loaded

`init.el` adds `lisp/` to `load-path`, then:

1. Requires `base` explicitly (must load first — sets up Evil, completion, eglot, magit).
2. Auto-requires every other `*.el` in `lisp/` alphabetically via:

```elisp
(let* ((dir   (expand-file-name "lisp" user-emacs-directory))
       (files (directory-files dir t "^[^._][^/]*\\.el$")))
  (dolist (file (sort files #'string<))
    (let ((feature (intern (file-name-base file))))
      (unless (eq feature 'base)
        (condition-case err
            (require feature)
          (error (message "Error requiring %s: %s" feature ...)))))))
```

Each module failure is non-fatal — it logs and continues.

### Module naming conventions

Files follow a `category-name.el` pattern:

- `lang-*.el` — language support (Ada, CL, OCaml, Odin)
- `tool-*.el` — tool/utility integration (Codex, Just, LanguageTool, Org, Roam)
- `ui-*.el` — UI enhancements (Dired)
- `base.el` — core layer, loaded first

Each file must end with `(provide 'feature-name)` matching the filename.

## Current Modules

| Module | Purpose |
|---|---|
| `base.el` | Evil, vertico/corfu/consult, magit, eglot, vterm/eshell, theme, leader keys |
| `lang-ada.el` | Ada via `ada-mode` + `alr` LSP |
| `lang-cl.el` | Common Lisp via SLY + Roswell; evil-cleverparens for structural editing |
| `lang-ocaml.el` | OCaml via `ocaml-ts-mode`/tuareg + `ocamllsp` |
| `lang-odin.el` | Odin via `odin-ts-mode` + OLS |
| `tool-codex.el` | Codex CLI integration with org-subtree sending |
| `tool-just.el` | Justfile syntax support |
| `tool-langtool.el` | LanguageTool grammar checking (conditional on JAR presence) |
| `tool-org.el` | Org mode tweaks (inline images, Markdown export) |
| `tool-roam.el` | Org-roam knowledge base |
| `ui-dired.el` | Dired icons, git info, auto-hide details |

## Adding a New Module

### 1. Create `lisp/category-name.el`

Follow this template:

```elisp
;;; category-name.el --- Short description  -*- lexical-binding: t -*-
;;; Commentary:
;; Longer description if needed.
;;; Code:

(use-package some-package
  :straight t          ; or (:type git :host github :repo "owner/repo")
  :defer t             ; prefer lazy loading where possible
  :commands (cmd1 cmd2)
  :mode ("\\.ext\\'" . some-mode)
  :hook (some-mode . some-setup-fn)
  :bind (("C-c x y" . some-cmd))
  :init
  ;; Runs BEFORE package loads — set variables here
  (setq some-var "value")
  :config
  ;; Runs AFTER package loads — further setup here
  (some-mode-setup))

(provide 'category-name)
;;; category-name.el ends here
```

The file is picked up automatically on next Emacs start — no changes to `init.el` needed.

### 2. Conditional loading patterns

```elisp
;; Only on GUI
:if (display-graphic-p)

;; Only if external tool exists
:if (executable-find "rg")

;; Only if a file/path exists
:when (file-exists-p "/path/to/jar")

;; Platform-specific
:if (memq window-system '(mac ns x))
```

### 3. Adding a new language

Use this pattern (see `lang-odin.el` or `lang-ada.el` as reference):

```elisp
;; File: lisp/lang-foo.el
(use-package foo-mode
  :straight (foo-mode :type git :host github :repo "author/foo-mode")
  :mode ("\\.foo\\'" . foo-mode)
  :hook (foo-mode . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(foo-mode . ("foo-lsp")))))

(provide 'lang-foo)
```

### 4. Adding to the SPC leader keymap

Leader keys are defined in `base.el` via `general.el`. To add bindings from a new module:

```elisp
(with-eval-after-load 'general
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   "x" '(:ignore t :which-key "mything")
   "xa" #'my-command))
```

Or use a `C-c` prefix for simpler additions:

```elisp
:bind (("C-c m a" . my-command-a)
       ("C-c m b" . my-command-b))
```

## Conventions

### Function naming

- `my/function` — public user-facing commands
- `module-name--helper` — private/internal helpers (double dash)
- `my/all-the-icons-dired-safe` — safety wrappers follow the `my/` prefix

### Variable/constant naming

- `tool-name--const-name` — module-local constants (double dash = private)
- Environment variables are read at load time with fallbacks:

```elisp
(defconst tool-name--path
  (or (getenv "TOOL_VAR")
      (expand-file-name "~/.local/bin/tool")))
```

### Keybinding namespaces in use

| Prefix | Area |
|---|---|
| `SPC f` | Files |
| `SPC b` | Buffers |
| `SPC w` | Windows |
| `SPC g` | Git (magit) |
| `SPC s` | Search |
| `SPC h` | Help |
| `SPC o` | Open (vterm, codex, org) |
| `SPC c` | Comment |
| `SPC e` | Edit (move lines) |
| `C-c c` | Codex CLI |
| `C-c n` | Org-roam |
| `C-c 4` | LanguageTool |
| `C-x g` | Magit status |

### Deferred configuration

Prefer `with-eval-after-load` over eager `:config` when extending packages defined elsewhere:

```elisp
;; Good — deferred, avoids loading eglot eagerly
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs ...))

;; Good — extends Evil state only after Evil loads
(with-eval-after-load 'evil
  (evil-set-initial-state 'my-mode 'insert))
```

## Package Manager

`straight.el` v6 is bootstrapped in `init.el`. Packages are pinned via lockfile at `straight/versions/default.el`. To update all packages: `M-x straight-pull-all`.

Packages from GitHub that aren't on MELPA use:

```elisp
:straight (pkg-name :type git :host github :repo "owner/repo")
```

Built-in packages use:

```elisp
:straight (:type built-in)
```

## Environment Variables Read at Startup

`exec-path-from-shell` is configured in `base.el` and only active in GUI mode (`:if (memq window-system '(mac ns x))`). It runs once on `after-init-hook` via:

```elisp
:hook (after-init . exec-path-from-shell-initialize)
```

Variables currently synced from the shell:

- `PATH`, `TMPDIR` — core (base.el)
- `OPENAI_API_KEY`, `JAVA_HOME`, `ORG_ROAM_DIR`, `LANGTOOL_JAR` — tools (base.el)
- `OPAM_SWITCH_PREFIX`, `OCAML_TOPLEVEL_PATH`, `CAML_LD_LIBRARY_PATH` — OCaml (lang-ocaml.el)

### Adding env vars from a new module

Use `with-eval-after-load` to append to `exec-path-from-shell-variables` **before** `exec-path-from-shell-initialize` fires. Because `exec-path-from-shell` is loaded eagerly in `base.el` and all modules load before `after-init-hook`, this runs at the right time and the vars are included in the single shell spawn:

```elisp
(with-eval-after-load 'exec-path-from-shell
  (dolist (var '("MY_VAR" "OTHER_VAR"))
    (add-to-list 'exec-path-from-shell-variables var)))
```

**Do not** call `exec-path-from-shell-copy-envs` from a module — it spawns a second shell process and is redundant with the `initialize` call already scheduled by `base.el`.
