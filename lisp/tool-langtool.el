(defconst tool-langtool--cmd-jar
  (let* ((env-jar (getenv "LANGTOOL_JAR"))
         (bin-dir (expand-file-name "~/.local/bin/"))
         (release-dir (expand-file-name "LanguageTool-6.6" bin-dir))
         (bin-jar (expand-file-name "languagetool-commandline.jar" bin-dir))
         (release-jar (expand-file-name "languagetool-commandline.jar" release-dir)))
    (expand-file-name (or (and env-jar (> (length env-jar) 0) env-jar)
                          (and (file-exists-p bin-jar) bin-jar)
                          release-jar)))
  "Absolute path to the LanguageTool command-line JAR.
Override via the LANGTOOL_JAR environment variable.")

(use-package langtool
  :straight t
  :when (file-exists-p tool-langtool--cmd-jar)
  :init
  (setq langtool-language-tool-jar tool-langtool--cmd-jar
        langtool-default-language "en-US")
  :bind (("C-c 4 c" . langtool-check)
         ("C-c 4 C" . langtool-correct-buffer)
         ("C-c 4 d" . langtool-check-done)))

(provide 'tool-langtool)

;;; tool-langtool.el ends here
