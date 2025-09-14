;;; lang-odin.el --- Odin language support with tree-sitter and Eglot -*- lexical-binding: t; -*-

(with-eval-after-load 'treesit
  (add-to-list 'treesit-language-source-alist
               '(odin "https://github.com/tree-sitter-grammars/tree-sitter-odin")))

(use-package odin-ts-mode
  :straight (odin-ts-mode :type git :host github :repo "Sampie159/odin-ts-mode")
  :mode ("\\.odin\\'" . odin-ts-mode)
  :hook (odin-ts-mode . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(odin-ts-mode . ("ols"))))

  ;; Formatting with odinfmt
  (defun odin-format-buffer ()
    "Format the current buffer with odinfmt."
    (interactive)
    (when (and buffer-file-name (eq major-mode 'odin-ts-mode))
      (save-buffer)
      (let ((exit-code (call-process "odinfmt" nil "*odinfmt-output*" nil buffer-file-name)))
        (if (eq exit-code 0)
            (revert-buffer :ignore-auto :noconfirm)
          (message "odinfmt failed — see *odinfmt-output* buffer")))))

  ;; Keybinding: C-c C-o to format the buffer
  (define-key odin-ts-mode-map (kbd "C-c C-o") #'odin-format-buffer))

(provide 'lang-odin)

;;; lang-odin.el ends here

