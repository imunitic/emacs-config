;;; lang-cl.el --- Common Lisp development environment -*- lexical-binding: t; -*-

(eval-when-compile
  (declare-function sly-edit-definition "sly")
  (declare-function sly-autodoc--fontify "sly-autodoc")
  (declare-function sly-current-connection "sly")
  (declare-function sly-flash-region "sly")
  ;; SLY sometimes references slime-mode for compatibility; declare to hush:
  (declare-function slime-mode "ext:slime"))

(use-package sly
  :init
  ;; Your Roswell setup
  (setq inferior-lisp-program "ros -Q run")
  (setq sly-lisp-implementations
        `((roswell ("ros" "-Q" "run"))))
  (setq sly-complete-symbol-function 'sly-simple-completions)

  ;; Make minibuffer prompts stay vanilla (less modal friction)
  (setq evil-collection-setup-minibuffer nil)

  ;; Optional: start REPL in insert state (feels natural for typing forms)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'sly-mrepl-mode 'insert))

  ;; Wire up Evil-style keys across SLY buffers (inspector, debug, xref, etc.)
  (with-eval-after-load 'evil-collection
    (ignore-errors (evil-collection-sly-setup)))

  ;; If you use Paredit / evil-cleverparens, enable them in the REPL too
  (with-eval-after-load 'evil-cleverparens
    (add-hook 'sly-mrepl-mode-hook #'evil-cleverparens-mode))

  :hook
  ;; Keep your original editing-mode activation in Lisp buffers
  (lisp-mode . sly-editing-mode)

  :config
  ;; (Place any extra SLY customizations here if needed)
)

(use-package sly-quicklisp
  :after sly)

(use-package evil-cleverparens
  :after evil
  :commands evil-cleverparens-mode
  :hook ((emacs-lisp-mode lisp-mode lisp-data-mode scheme-mode) . evil-cleverparens-mode)
  :config
  (setq evil-cleverparens-use-additional-movement-keys t
        evil-cleverparens-swap-move-by-word-and-symbol t))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(provide 'lang-cl)
;;; lang-cl.el ends here
