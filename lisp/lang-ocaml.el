;;; lang-ocaml.el --- OCaml support with opam, dune, merlin, and ocaml-lsp -*- lexical-binding: t; -*-

(when (featurep 'exec-path-from-shell)
  ;; Extend just for OCaml-specific vars
  (exec-path-from-shell-copy-envs
   '("OPAM_SWITCH_PREFIX" "OCAML_TOPLEVEL_PATH" "CAML_LD_LIBRARY_PATH")))

;; ocamlformat
(use-package ocamlformat)

;; tuareg for older OCaml files
(use-package tuareg
  :hook (tuareg-mode . merlin-mode))

;; merlin (type info, jump to def, etc.)
(use-package merlin
  :hook ((tuareg-mode . merlin-mode)
         (caml-mode . merlin-mode)
         (reason-mode . merlin-mode))
  :config
  (setq merlin-command "ocamlmerlin"))

;; ocaml-lsp with eglot
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(tuareg-mode . ("ocamllsp")))
  (add-to-list 'eglot-server-programs '(reason-mode . ("ocamllsp"))))

;; dune integration
(use-package dune)

(provide 'lang-ocaml)
;;; lang-ocaml.el ends here
