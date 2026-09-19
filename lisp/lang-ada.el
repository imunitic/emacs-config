;;; lang-ada.el --- description -*- lexical-binding: t; -*-
;; Silence compile-time "unknown function" by declaring them
(eval-when-compile
  (declare-function wisi-in-comment-p "wisi")
  (declare-function eglot--server-capable "eglot"))

(use-package wisi :defer t)            ;; wisi is a dependency of ada-mode

;; Treat any directory containing alire.toml as its own project root,
;; even nested inside a bigger git-tracked tree (e.g. Development/Ada/
;; is one git repo, but Development/Ada/raytracer/ is its own alr
;; crate/project). Without this, `project-current' walks up to the
;; outer .git root, and eglot roots ada_language_server there instead
;; of at the crate -- ALS then can't find the .gpr file and its
;; `initialize' request just hangs/times out.
(with-eval-after-load 'project
  (add-to-list 'project-vc-extra-root-markers "alire.toml"))

;; ada-mode finds ada_language_server itself via `gnat-find-als' and
;; starts eglot through its own `ada-eglot-setup' machinery --
;; forcing the four backends below to 'eglot is what triggers that,
;; instead of the generic `eglot-ensure'/`eglot-server-programs'
;; path, which doesn't know how to hand ALS the right .gpr file or
;; GPR_PROJECT_PATH.
;;
;; `gnat-lsp-server-exec' pins the exact binary rather than leaving
;; `gnat-find-als' to scan PATH/`~/.alire/bin': the `alr install
;; ada_language_server` release there is a stale 25.0.0 build (Aug
;; 2025) that crashes internally on every `initialize' request
;; (`CONSTRAINT_ERROR : lsp-servers.adb:922 access check failed`,
;; confirmed independent of Emacs by feeding it a raw LSP request
;; from the shell). A fresh 26.0.0 works fine, but `alr -n -f install
;; ada_language_server` can't currently complete -- it hits an
;; internal Alire bug (`Alire.Milestones.Containers.Sets.First_Element:
;; set is empty') trying to replace the conflicting old
;; libadalang_tools=25.0.0 release in the shared install prefix. So
;; for now: `alr get ada_language_server && alr build' in a scratch
;; dir, then move the built binary out of the way of that broken
;; install-prefix machinery -- see ~/.alire/dev-builds/. Revisit once
;; the Alire bug is fixed upstream or the old release is untangled;
;; at that point this can go back to plain `gnat-find-als' discovery.
;;
;; GPR_PROJECT_PATH (for `with "aunit";` and friends) comes from each
;; project's own .envrc (`eval "$(alr -n -q printenv --unix)"`) via
;; envrc-mode, same pattern as the OCaml/opam setup in lang-ocaml.el.
;; The .gpr file itself comes from that project's .dir-locals.el
;; setting `ada-eglot-gpr-file'.
(use-package ada-mode
  :mode ("\\.ad[bs]\\'" . ada-mode)
  :custom
  (gnat-lsp-server-exec
   (expand-file-name
    "~/.alire/dev-builds/ada_language_server_26.0.0_46c28e2c/.obj/server/ada_language_server"))
  (ada-diagnostics-backend 'eglot)
  (ada-face-backend 'eglot)
  (ada-indent-backend 'eglot)
  (ada-xref-backend 'eglot))
  ;; ada-statement-backend can't be 'eglot; it stays whatever ada-mode
  ;; defaults it to (wisi if ada_mode_wisi_lr1_parse is installed and
  ;; on PATH, otherwise 'none -- statement motion commands unavailable).

(provide 'lang-ada)
