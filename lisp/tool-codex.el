;;; tool-codex.el --- Codex CLI integration helpers -*- lexical-binding: t; -*-

(use-package codex-cli
  :straight (codex-cli :type git :host github :repo "emacsmirror/codex-cli")
  :commands (codex-cli-toggle
             codex-cli-start
             codex-cli-stop
             codex-cli-stop-all
             codex-cli-send-prompt
             codex-cli-send-region
             codex-cli-send-file
             codex-cli-toggle-all
             codex-cli-toggle-all-next-page
             codex-cli-toggle-all-prev-page)
  :bind (("C-c c t" . codex-cli-toggle)
         ("C-c c s" . codex-cli-start)
         ("C-c c q" . codex-cli-stop)
         ("C-c c Q" . codex-cli-stop-all)
         ("C-c c p" . codex-cli-send-prompt)
         ("C-c c r" . codex-cli-send-region)
         ("C-c c f" . codex-cli-send-file)
         ("C-c c a" . codex-cli-toggle-all)
         ("C-c c n" . codex-cli-toggle-all-next-page)
         ("C-c c b" . codex-cli-toggle-all-prev-page))
  :init
  (setq codex-cli-executable "codex"
        codex-cli-terminal-backend 'vterm
        codex-cli-side 'right
        codex-cli-width 90
        codex-cli-focus-on-open t
        codex-cli-log-injections t))

(provide 'tool-codex)

;;; tool-codex.el ends here
