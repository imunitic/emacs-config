;;; tool-roam.el --- Org Roam -*- lexical-binding: t; -*-

(defconst tool-roam--notes-dir
  (let* ((env-dir (getenv "ORG_ROAM_DIR"))
         (legacy "~/Development/documents/org/roam")
         (fallback (expand-file-name "org-roam" (or (getenv "ORG_HOME") "~")))
         (path (cond
                (env-dir env-dir)
                ((file-directory-p (expand-file-name legacy)) legacy)
                (t fallback))))
    (expand-file-name path))
  "Absolute path to the Org-roam notes directory.
Override via the ORG_ROAM_DIR environment variable.")

(defconst tool-roam--db-path
  (expand-file-name "org-roam.db" tool-roam--notes-dir)
  "Absolute path to the Org-roam SQLite database.")

(unless (file-directory-p tool-roam--notes-dir)
  (make-directory tool-roam--notes-dir t))

(use-package org-roam
  :straight t
  :defer t
  :commands (org-roam-buffer-toggle
             org-roam-node-find
             org-roam-node-insert
             org-roam-graph
             org-roam-capture
             org-roam-dailies-capture-today)
  :init
  (setq org-roam-directory tool-roam--notes-dir
        org-roam-db-location tool-roam--db-path)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(provide 'tool-roam)

;;; tool-roam.el ends here
