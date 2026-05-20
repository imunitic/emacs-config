;;; tool-ghostel.el --- Ghostel terminal (libghostty) integration  -*- lexical-binding: t -*-
;;; Commentary:
;; Ghostel is a modern terminal emulator for Emacs powered by libghostty-vt
;; (the same VT engine as the Ghostty terminal).  Drop-in alternative to
;; vterm with Kitty keyboard/mouse protocols, OSC 8 hyperlinks, and
;; auto-injected shell integration for bash/zsh/fish.
;;
;; First launch downloads the native module automatically; if that fails,
;; run M-x ghostel-download-module or M-x ghostel-module-compile.
;;
;; Entry points:
;;   M-x ghostel             — open a terminal buffer
;;   M-x ghostel-project     — open one in the current project root
;;   M-x my/ghostel-split    — open in a vertical/horizontal split
;;; Code:

(defun tool-ghostel--disable-line-numbers ()
  "Turn off `display-line-numbers-mode' in ghostel buffers."
  (display-line-numbers-mode 0))

(defun my/ghostel-split (&optional arg)
  "Open a ghostel terminal in a split.
With no ARG, split vertically (right).  With C-u ARG, split horizontally (below)."
  (interactive "P")
  (if arg
      (split-window-below)
    (split-window-right))
  (other-window 1)
  (ghostel))

(use-package ghostel
  :straight (ghostel :type git :host github :repo "dakra/ghostel"
                     :files ("lisp/*.el"
                             ("etc/terminfo"    "etc/terminfo/*.terminfo")
                             ("etc/terminfo/67" "etc/terminfo/67/*")
                             ("etc/terminfo/g"  "etc/terminfo/g/*")
                             ("etc/terminfo/x"  "etc/terminfo/x/*")
                             ("etc/terminfo/78" "etc/terminfo/78/*")))
  :if (memq system-type '(gnu gnu/linux darwin berkeley-unix))
  :commands (ghostel
             ghostel-project
             ghostel-next
             ghostel-previous
             ghostel-clear
             ghostel-download-module
             ghostel-module-compile)
  :hook (ghostel-mode . tool-ghostel--disable-line-numbers)
  :config
  ;; Match vterm's palette: vterm inherits from `term-color-*' (which the
  ;; ample-flat theme tints brightly), but ghostel inherits from
  ;; `ansi-color-*' (which the theme does not customize).  Remap so both
  ;; terminals render ANSI colors identically.  vterm pulls term.el in
  ;; for us; ghostel does not, so require it explicitly or the inherit
  ;; targets won't exist and faces render uncolored.
  (require 'term)
  (dolist (c '(black red green yellow blue magenta cyan white))
    (set-face-attribute (intern (format "ghostel-color-%s" c)) nil
                        :inherit (intern (format "term-color-%s" c)))
    (set-face-attribute (intern (format "ghostel-color-bright-%s" c)) nil
                        :inherit (intern (format "term-color-bright-%s" c)))))

(use-package evil-ghostel
  :straight (evil-ghostel :type git :host github :repo "dakra/ghostel"
                          :local-repo "ghostel"
                          :files ("extensions/evil-ghostel/*.el"))
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

(provide 'tool-ghostel)
;;; tool-ghostel.el ends here
