;;; tool-dired.el --- Dired enhancements and subtree support -*- lexical-binding: t -*-
;;; Commentary:
;; Adds dired-subtree and related enhancements.
;;; Code:

(use-package dired-subtree
  :straight t
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle))
  :config
  (setq dired-subtree-use-backgrounds nil))

(provide 'tool-dired)
;;; tool-dired.el ends here
