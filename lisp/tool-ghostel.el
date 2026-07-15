;;; tool-ghostel.el --- Ghostel terminal (libghostty) integration  -*- lexical-binding: t -*-
;;; Commentary:
;; Ghostel is a terminal emulator for Emacs powered by libghostty-vt (the
;; same VT engine as the Ghostty terminal).  It saves alternate-screen
;; content in its scrollback, which vterm/libvterm does not.
;;
;; ghostel-term is set to xterm-256color instead of the default
;; xterm-ghostty.  This suppresses TERM_PROGRAM=ghostty so claude-code does
;; not activate the Kitty keyboard protocol, which caused rendering
;; artifacts previously.  libghostty-vt still saves alternate-screen
;; scrollback regardless of the advertised TERM.
;;; Code:

(use-package ghostel
  :straight (ghostel :type git :host github :repo "dakra/ghostel"
                     :files ("lisp/*.el"
                             ("etc/terminfo"    "etc/terminfo/*.terminfo")
                             ("etc/terminfo/67" "etc/terminfo/67/*")
                             ("etc/terminfo/g"  "etc/terminfo/g/*")
                             ("etc/terminfo/x"  "etc/terminfo/x/*")
                             ("etc/terminfo/78" "etc/terminfo/78/*")))
  :if (memq system-type '(gnu gnu/linux darwin berkeley-unix))
  :commands (ghostel ghostel-project)
  :hook ((ghostel-mode . (lambda () (display-line-numbers-mode 0)))
         (ghostel-mode . my/terminal-nobreak-space-fix))
  :init
  ;; Use plain xterm-256color instead of xterm-ghostty so TERM_PROGRAM=ghostty
  ;; is never advertised — prevents claude-code from activating Kitty keyboard
  ;; protocol (which caused rendering artifacts when ghostel was removed).
  (setq ghostel-term "xterm-256color")
  (setq ghostel-environment '("FORCE_COLOR=3"))
  ;; Registered via evil's own initial-state table (like term-mode/comint-mode
  ;; are by default), not a bare mode-hook — a hook only disables evil once, at
  ;; buffer creation, and evil has no memory of that afterwards. Any later
  ;; re-enable (e.g. evil's globalized mode re-asserting itself) would bring
  ;; evil-local-mode back in normal state with no persistent registration.
  ;; This way it re-lands in emacs-state every time evil (re-)initializes the
  ;; buffer's state, not just once.
  (with-eval-after-load 'evil
    (evil-set-initial-state 'ghostel-mode 'emacs))
  :config
  ;; ghostel inherits ANSI colors from `ansi-color-*' faces; the theme only
  ;; customizes `term-color-*'.  Remap so ghostel renders identically to vterm.
  (require 'term)
  (dolist (c '(black red green yellow blue magenta cyan white))
    (set-face-attribute (intern (format "ghostel-color-%s" c)) nil
                        :inherit (intern (format "term-color-%s" c)))
    (set-face-attribute (intern (format "ghostel-color-bright-%s" c)) nil
                        :inherit (intern (format "term-color-bright-%s" c)))))

(provide 'tool-ghostel)
;;; tool-ghostel.el ends here
