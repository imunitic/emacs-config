;;; tool-claude-code.el --- Claude Code CLI integration  -*- lexical-binding: t -*-
;;; Commentary:
;; Integrates Claude Code CLI into Emacs via claude-code.el.
;; Uses C-c a prefix to avoid clashing with C-c c (codex-cli).
;;; Code:

(use-package inheritenv
  :straight (:type git :host github :repo "purcell/inheritenv"))

(use-package claude-code
  :straight (claude-code :type git :host github :repo "stevemolitor/claude-code.el")
  :after inheritenv
  :commands (claude-code
             claude-code-start-in-directory
             claude-code-send-command
             claude-code-send-command-with-context
             claude-code-kill
             claude-code-toggle-read-only-mode)
  :bind-keymap ("C-c a" . claude-code-command-map)
  :init
  (setq claude-code-terminal-backend 'vterm)
  :config
  ;; Display claude buffers in a right-side vertical split at 1/3 frame width.
  ;; display-buffer-alist takes priority over action args, so this covers both
  ;; claude-code-display-window-fn and the hardcoded call in claude-code-toggle.
  (add-to-list 'display-buffer-alist
               '("\\*claude:"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.33))))

(provide 'tool-claude-code)
;;; tool-claude-code.el ends here
