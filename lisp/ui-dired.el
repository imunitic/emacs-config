;;; ui-dired.el --- Dired UI niceties -*- lexical-binding: t; -*-

(defun my/all-the-icons-dired-safe ()
  "Enable `all-the-icons-dired-mode' safely."
  (when (and (display-graphic-p)
             (not (file-remote-p default-directory))
             (find-font (font-spec :family "all-the-icons")))
    ;; Make sure the mode is defined before calling it
    (require 'all-the-icons-dired)
    (all-the-icons-dired-mode 1)))

(use-package all-the-icons-dired
  :after dired
  :commands all-the-icons-dired-mode
  :init
  ;; Attach directly to dired-mode-hook
  (add-hook 'dired-mode-hook #'my/all-the-icons-dired-safe))

(use-package dired-git-info
  :after dired
  :commands dired-git-info-mode
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode))

(add-hook 'dired-mode-hook
  (lambda ()
    ;; evil-collection puts dired in normal state and binds SPC to
    ;; dired-next-line in dired-mode-map's normal-state auxiliary keymap,
    ;; shadowing our leader prefix.  Override it buffer-locally so SPC
    ;; triggers the leader instead.
    (when-let ((leader (lookup-key evil-normal-state-map (kbd "SPC"))))
      (evil-local-set-key 'normal (kbd "SPC") leader))))

(provide 'ui-dired)
;;; ui-dired.el ends here
