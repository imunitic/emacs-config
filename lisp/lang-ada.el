;; Silence compile-time "unknown function" by declaring them
(eval-when-compile
  (declare-function wisi-in-comment-p "wisi")
  (declare-function eglot--server-capable "eglot"))

(use-package wisi :defer t)            ;; wisi is a dependency of ada-mode
(use-package ada-mode
  :mode ("\\.ad[bs]\\'" . ada-mode)
  :hook (ada-mode . eglot-ensure)
  :config
  ;; Add Ada language server (via alr)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(ada-mode .
                   ("alr" "exec" "--" "ada_language_server"
                    "--tracefile" ,(expand-file-name "~/tmp/als.log"))))))
(provide 'lang-ada)

