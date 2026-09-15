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

(defun tool-ghostel--speed-up-buffer ()
  "Trim redisplay work that `ghostel-mode' does not already disable.
Ghostel's own mode body already covers undo, font-lock, truncation,
scrolling and hl-line; these are the remaining settings from
https://www.jamescherti.com/emacs-terminal-performance-vterm-eat-ansi-term-ghostel/
that it leaves alone."
  ;; Terminal output is never bidirectional -- skip the bidi reordering
  ;; and bracket-pair scan Emacs otherwise runs over every redrawn line.
  (setq-local bidi-paragraph-direction 'left-to-right)
  (setq-local bidi-inhibit-bpa t)
  (setq-local fast-but-imprecise-scrolling t)
  (setq-local redisplay-skip-fontification-on-input t)
  ;; Render U+00A0 as a plain space.  Emacs otherwise applies the
  ;; nobreak-space face (underlined by default) to NBSP in buffer text,
  ;; making it look like _ in colored regions (e.g. ccstatusline output).
  (setq-local nobreak-char-display nil))

;; Completion in a terminal is the shell's job, but `global-corfu-mode'
;; re-enables `corfu-mode' from `after-change-major-mode-hook', which runs
;; after `ghostel-mode' and its hooks -- so switching it off in
;; `tool-ghostel--speed-up-buffer' does not stick.  Opt out through corfu's
;; own exclusion list instead.
(with-eval-after-load 'corfu
  (setq global-corfu-modes '((not ghostel-mode) t)))

(use-package ghostel
  :straight (ghostel :type git :host github :repo "dakra/ghostel"
                     :files ("lisp/*.el"
                             ("etc/terminfo"    "etc/terminfo/*.terminfo")
                             ("etc/terminfo/67" "etc/terminfo/67/*")
                             ("etc/terminfo/g"  "etc/terminfo/g/*")
                             ("etc/terminfo/x"  "etc/terminfo/x/*")
                             ("etc/terminfo/78" "etc/terminfo/78/*")
                             ;; Shell integration (OSC 7 dirtrack, ghostel_cmd) --
                             ;; without this, the auto-injected ZDOTDIR/ENV shim
                             ;; sources a script that isn't in the build dir, and
                             ;; fails silently: default-directory never leaves ~/.
                             ("etc/shell" "etc/shell/ghostel.*")
                             ("etc/shell/bootstrap/bash" "etc/shell/bootstrap/bash/*")
                             ("etc/shell/bootstrap/fish/vendor_conf.d" "etc/shell/bootstrap/fish/vendor_conf.d/*")
                             ("etc/shell/bootstrap/zsh/.zshenv" . "etc/shell/bootstrap/zsh/.zshenv")))
  :if (memq system-type '(gnu gnu/linux darwin berkeley-unix))
  :commands (ghostel ghostel-project)
  :hook (ghostel-mode . tool-ghostel--speed-up-buffer)
  :init
  ;; Use plain xterm-256color instead of xterm-ghostty so TERM_PROGRAM=ghostty
  ;; is never advertised — prevents claude-code from activating Kitty keyboard
  ;; protocol (which caused rendering artifacts when ghostel was removed).
  (setq ghostel-term "xterm-256color")
  (setq ghostel-environment '("FORCE_COLOR=3"))
  ;; Redraw ~100fps instead of the 0.033 (~30fps) default, so bursts of
  ;; output land sooner.  `ghostel-adaptive-fps' still throttles sustained
  ;; output, so this mainly affects short bursts.
  (setq ghostel-timer-delay 0.01)
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
