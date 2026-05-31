;;; lang-ocaml.el --- OCaml support  -*- lexical-binding: t; -*-

(with-eval-after-load 'treesit
  (add-to-list 'treesit-language-source-alist
               '(ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" nil "grammars/ocaml/src"))
  (add-to-list 'treesit-language-source-alist
               '(ocaml-interface "https://github.com/tree-sitter/tree-sitter-ocaml" nil "grammars/interface/src")))

(with-eval-after-load 'exec-path-from-shell
  (dolist (var '("OPAM_SWITCH_PREFIX" "OCAML_TOPLEVEL_PATH" "CAML_LD_LIBRARY_PATH"))
    (add-to-list 'exec-path-from-shell-variables var)))

(use-package ocaml-ts-mode
  :straight t
  :mode (("\\.ml\\'"  . ocaml-ts-mode)
         ("\\.mli\\'" . ocaml-interface-ts-mode)))

(use-package ocamlformat
  :straight t
  :hook (ocaml-ts-mode . (lambda ()
                           (add-hook 'before-save-hook #'ocamlformat-before-save nil t))))

(use-package dune
  :straight t)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((ocaml-ts-mode ocaml-interface-ts-mode) . ("ocamllsp"))))

(with-eval-after-load 'ocaml-ts-mode
  (add-hook 'ocaml-ts-mode-hook #'eglot-ensure)
  (add-hook 'ocaml-interface-ts-mode-hook #'eglot-ensure)
  ;; append so eglot-ensure fires first and loads eglot before this runs
  (add-hook 'ocaml-ts-mode-hook
            (lambda () (when (fboundp 'eglot-inlay-hints-mode)
                         (eglot-inlay-hints-mode 1)))
            t)
  (add-hook 'ocaml-interface-ts-mode-hook
            (lambda () (when (fboundp 'eglot-inlay-hints-mode)
                         (eglot-inlay-hints-mode 1)))
            t))

(provide 'lang-ocaml)
;;; lang-ocaml.el ends here
