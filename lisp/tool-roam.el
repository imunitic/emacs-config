;;; tool-roam.el --- Org Roam -*- lexical-binding: t; -*-

(defun org-roam-node-directory (node)
  "Return NODE's subdirectory relative to `org-roam-directory', or \"\" at the root.
Custom accessor (not a real struct slot) consumed by
`org-roam-node-display-template' via \"${directory}\" -- see
`org-roam-node--format-entry', which resolves any \"${field}\" to a call to
`org-roam-node-field'."
  (let ((rel (directory-file-name
              (file-relative-name (file-name-directory (org-roam-node-file node))
                                   org-roam-directory))))
    (if (string= rel ".") "" rel)))

(defun tool-roam--resolve-notes-dir ()
  "Return the absolute path to the Org-roam notes directory.
Reads ORG_ROAM_DIR at call time so exec-path-from-shell has already run."
  (let* ((env-dir (getenv "ORG_ROAM_DIR"))
         (legacy "~/Development/documents/org/roam")
         (fallback (expand-file-name "org-roam" (or (getenv "ORG_HOME") "~")))
         (path (cond
                (env-dir env-dir)
                ((file-directory-p (expand-file-name legacy)) legacy)
                (t fallback))))
    (expand-file-name path)))

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
  (add-hook 'after-init-hook
            (lambda ()
              (let ((dir (tool-roam--resolve-notes-dir)))
                (unless (file-directory-p dir)
                  (make-directory dir t))
                (setq org-roam-directory dir
                      org-roam-db-location (expand-file-name "org-roam.db" dir))))
            t)  ; append — run after exec-path-from-shell-initialize
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template
        (concat "${directory:10} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  ;; `dot' (the org-roam-graph default) lays the graph out as a strict
  ;; hierarchy, which produces a very wide, minimal-height SVG when the note
  ;; graph is bushy rather than deep. `neato' is Graphviz's classic
  ;; stress-majorization "spring" layout -- it sizes width/height from actual
  ;; connectivity and, unlike `sfdp' (built for graphs with thousands of
  ;; nodes, trading layout quality for speed), fully converges to a compact
  ;; result at the size of a personal note vault. Falls back to `dot' if
  ;; Graphviz's extra layout engines aren't on PATH.
  (setq org-roam-graph-executable (if (executable-find "neato") "neato" "dot")
        org-roam-graph-extra-config '(("overlap" . "false")
                                       ("splines" . "true")))
  ;; org-roam-graph graphs every link type it finds, not just note-to-note
  ;; (`id') links -- by default only bare `file' links are hidden. Hide http/
  ;; https too so external URLs a note references don't show up as graph
  ;; nodes alongside actual notes.
  (setq org-roam-graph-link-hidden-types '("file" "http" "https"))
  ;; Long note titles otherwise stretch each node into a wide, thin ellipse.
  ;; Wrap at 30 chars instead of truncating at the (100-char) default so
  ;; titles break onto multiple lines and nodes stay roughly square.
  (setq org-roam-graph-shorten-titles 'wrap
        org-roam-graph-max-title-length 30)
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  ;; Optional machine-local directory/category layout — gitignored, see
  ;; lisp/_tool-roam-local.el. Absent on a fresh clone; silently skipped.
  (require '_tool-roam-local nil t))

(provide 'tool-roam)

;;; tool-roam.el ends here
