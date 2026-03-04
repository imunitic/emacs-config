;;; tool-langtool.el --- description -*- lexical-binding: t; -*-
(defun tool-langtool--find-jar ()
  "Return the LanguageTool JAR path, checking env var and known locations."
  (let* ((env-jar (getenv "LANGTOOL_JAR"))
         (bin-dir (expand-file-name "~/.local/bin/"))
         (candidates
          (list (and env-jar (> (length env-jar) 0) env-jar)
                (expand-file-name "langtool.jar" bin-dir)
                (expand-file-name "languagetool-commandline.jar" bin-dir)
                (expand-file-name "LanguageTool-6.6/languagetool-commandline.jar" bin-dir))))
    (seq-find #'file-exists-p (delq nil candidates))))

(use-package langtool
  :straight t
  :when (tool-langtool--find-jar)
  :init
  (setq langtool-language-tool-jar (tool-langtool--find-jar)
        langtool-default-language "en-US")
  :bind (("C-c 4 c" . langtool-check)
         ("C-c 4 C" . langtool-correct-buffer)
         ("C-c 4 o" . langtool-correct-at-point)
         ("C-c 4 d" . langtool-check-done)))

(provide 'tool-langtool)

;;; tool-langtool.el ends here
