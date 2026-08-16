;;; lang-zig.el --- Zig language support with Eglot -*- lexical-binding: t; -*-

(use-package zig-mode
  :straight t
  :mode ("\\.\\(?:zig\\|zon\\)\\'" . zig-mode))

;; Register zls lazily, the same way lang-ocaml.el registers ocamllsp.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(zig-mode . ("zls"))))

;; Activate eglot only once zig-mode loads, keeping startup lean.
(with-eval-after-load 'zig-mode
  (add-hook 'zig-mode-hook #'eglot-ensure))

(provide 'lang-zig)
;;; lang-zig.el ends here