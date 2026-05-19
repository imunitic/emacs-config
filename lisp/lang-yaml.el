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

(provide 'lang-yaml)

;;; lang-yaml.el ends here
