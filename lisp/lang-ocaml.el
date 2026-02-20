;;; lang-ocaml.el --- OCaml support with opam, dune, merlin, and ocaml-lsp -*- lexical-binding: t; -*-

(with-eval-after-load 'exec-path-from-shell
  ;; Add OCaml vars before exec-path-from-shell-initialize fires on after-init-hook
  (dolist (var '("OPAM_SWITCH_PREFIX" "OCAML_TOPLEVEL_PATH" "CAML_LD_LIBRARY_PATH"))
    (add-to-list 'exec-path-from-shell-variables var)))

;; Prefer ocaml-ts-mode if available (Emacs 29+)
(when (fboundp 'ocaml-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.ml\\'"  . ocaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.mli\\'" . ocaml-ts-mode)))

;; ocamlformat
(use-package ocamlformat)

;; tuareg for older OCaml files
(use-package tuareg)  ;; (no merlin hook)

;; merlin installed but not auto-enabled (ocamllsp provides LSP features)
(use-package merlin
  :config
  (setq merlin-command "ocamlmerlin"))

;; ocaml-lsp with eglot (auto-start)
(defvar eglot-server-programs nil)  ;; avoid "void variable" on add-to-list
(use-package eglot
  :hook ((ocaml-ts-mode . eglot-ensure)
         (tuareg-mode   . eglot-ensure)
         (reason-mode   . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(tuareg-mode   . ("ocamllsp")))
  (add-to-list 'eglot-server-programs '(ocaml-ts-mode . ("ocamllsp")))
  (add-to-list 'eglot-server-programs '(reason-mode   . ("ocamllsp"))))

;; dune integration
(use-package dune)

(provide 'lang-ocaml)
;;; lang-ocaml.el ends here
