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
  (global-set-key (kbd "C-c C-x C-v") #'org-toggle-inline-images))

(provide 'tool-org)
