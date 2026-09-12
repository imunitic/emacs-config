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

;; zig-mode's auto-mode-alist regex (\.\(?:zig\|zon\)\') doesn't match the
;; simple `\.ext\'' shape semantic-symref tries to auto-derive, so
;; xref-find-references' grep fallback errors out unless we register the
;; glob patterns explicitly.
(with-eval-after-load 'semantic/symref/grep
  (add-to-list 'semantic-symref-filepattern-alist '(zig-mode "*.zig" "*.zon")))

(provide 'lang-zig)
;;; lang-zig.el ends here
