;;; tool-org.el --- description -*- lexical-binding: t; -*-
(use-package org
  :straight (:type built-in)
  :hook
  ;; Automatically show inline images when entering org-mode
  (org-mode . org-display-inline-images)
  ;; Automatically refresh inline images after src block execution
  (org-babel-after-execute . org-display-inline-images)
  :config
  (require 'ox-md) ;; Markdown exporter bundled with built-in Org
  ;; Optional: toggle key for hiding images when needed
  (global-set-key (kbd "C-c C-x C-v") #'org-toggle-inline-images)

  ;; TODO keyword sequence with shortcut keys
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "BLOCKED(b)" "REVIEW(r)"
                    "|" "DONE(d)" "CANCELLED(c)")))

  ;; Faces matched to ample-flat palette
  (setq org-todo-keyword-faces
        '(("TODO"        . (:foreground "#6C9EF8" :weight bold))
          ("IN-PROGRESS" . (:foreground "#F0C674" :weight bold))
          ("BLOCKED"     . (:foreground "#CC6666" :weight bold))
          ("REVIEW"      . (:foreground "#B294BB" :weight bold))
          ("DONE"        . (:foreground "#8ABA6A" :weight bold))
          ("CANCELLED"   . (:foreground "#666666" :weight bold :strike-through t))))

  ;; Keybinding for copying TODO items with file path
  (define-key org-mode-map (kbd "C-c o c") #'tool-org-copy-todo-with-path))

(defun tool-org-copy-todo-with-path ()
  "Copy current Org TODO item to clipboard with full file path."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let (start end)
    (save-excursion
      (org-back-to-heading t)
      (setq start (point))
      (org-end-of-subtree t t)
      (setq end (point)))
    (let ((todo-text (buffer-substring-no-properties start end))
          (file-path buffer-file-name))
      (with-temp-buffer
        (insert (format "# %s\n%s" file-path todo-text))
        (clipboard-kill-region (point-min) (point-max)))
      (message "Copied TODO item with file path to clipboard"))))

(provide 'tool-org)
