;;; lang-just.el --- Justfile support -*- lexical-binding: t; -*-

(use-package just-mode
  :mode ("justfile\\'" . just-mode)
  :config
  ;; Optional: use shell-script-mode features
  (add-hook 'just-mode-hook #'sh-mode)
  ;; Optional: set indentation level
  (setq just-indent-offset 2))

(provide 'tool-just)
;;; lang-just.el ends here
