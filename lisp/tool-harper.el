;;; tool-harper.el --- Harper grammar/spell LSP integration  -*- lexical-binding: t -*-
;;; Commentary:
;; Lean grammar and spell checking via harper-ls.
;; harper-ls speaks LSP so eglot handles the protocol — no extra package needed.
;; Errors appear as flymake diagnostics inline in the buffer.
;;; Code:

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((text-mode markdown-mode org-mode git-commit-mode)
                 . ("harper-ls" "--stdio"))))

(defun tool-harper--maybe-enable ()
  "Start harper-ls via eglot if the binary is available."
  (when (executable-find "harper-ls")
    (eglot-ensure)))

(add-hook 'text-mode-hook     #'tool-harper--maybe-enable)
(add-hook 'markdown-mode-hook #'tool-harper--maybe-enable)
(add-hook 'org-mode-hook      #'tool-harper--maybe-enable)

(provide 'tool-harper)
;;; tool-harper.el ends here
