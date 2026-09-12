;;; lang-yaml.el --- YAML support (k8s, CI, configs) -*- lexical-binding: t; -*-
;;; Commentary:
;; Simple YAML editing via yaml-mode. Covers .yaml/.yml plus common
;; extensionless files (Kubernetes manifests, Helm charts, kustomize).
;;; Code:

(use-package yaml-mode
  :straight t
  :mode (("\\.ya?ml\\'" . yaml-mode)
         ("kustomization\\'" . yaml-mode)
         ("Chart\\'" . yaml-mode))
  :hook (yaml-mode . (lambda ()
                       (setq-local tab-width 2)
                       (setq-local indent-tabs-mode nil))))

;; auto-mode-alist's "\.ya?ml\'" means "optional i" in regex terms, but
;; semantic-symref pastes it into a shell glob where "?" is a mandatory
;; single-char wildcard, so the derived "*.ya?ml" never matches ".yaml".
;; Register the real extensions explicitly so xref-find-references works.
(with-eval-after-load 'semantic/symref/grep
  (add-to-list 'semantic-symref-filepattern-alist '(yaml-mode "*.yaml" "*.yml")))

(provide 'lang-yaml)

;;; lang-yaml.el ends here
