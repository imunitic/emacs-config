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

(defun my/harper-start ()
  "Start harper-ls in the current buffer via eglot."
  (interactive)
  (if (executable-find "harper-ls")
      (eglot-ensure)
    (message "harper-ls not found in PATH")))

(defun my/harper-stop ()
  "Stop harper-ls in the current buffer."
  (interactive)
  (if (eglot-current-server)
      (eglot-shutdown (eglot-current-server))
    (message "No active eglot server in this buffer")))

(global-set-key (kbd "C-c 4 c") #'my/harper-start)
(global-set-key (kbd "C-c 4 d") #'my/harper-stop)

(provide 'tool-harper)
;;; tool-harper.el ends here
