(use-package ample-theme
  :init (progn (load-theme 'ample t t)
               (load-theme 'ample-flat t t)
               (load-theme 'ample-light t t)
               (enable-theme 'ample-flat))
  :defer t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init
  (setq exec-path-from-shell-variables
	'("PATH" "MANPATH" "TMPDIR"))
  :config
  (exec-path-from-shell-initialize))

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (setq all-the-icons-dired-monochrome nil))

(use-package magit
  :commands (magit-status magit-dispatch)
  :init
  (with-eval-after-load 'evil-collection
    (setq evil-collection-magit-use-aggressive-keys t))
  :bind (("C-x g" . magit)
	   :map magit-file-section-map
           ("RET" . magit-diff-visit-file-other-window)
           :map magit-hunk-section-map
           ("RET" . magit-diff-visit-file-other-window)))

;; base.el
(use-package eglot
  :defer t
  :hook
  ;; Add Eglot to general prog-mode (but you can restrict if you prefer)
  ((c-mode c++-mode python-mode rust-mode go-mode) . eglot-ensure)

  :config
  ;; Silence noisy messages, optional
  (setq eglot-extend-to-xref t))

(use-package vterm
  :config
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0))))

;; Powerline
(use-package powerline
  :config
  (powerline-default-theme))

;; Buffers
(setq switch-to-buffer-obey-display-actions t)

;; eshell
(use-package eshell
  :config
  (setq eshell-destroy-buffer-when-process-dies t)
  (with-eval-after-load 'em-term
    (add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode 0)))
    (add-to-list 'eshell-visual-commands "lazygit")
    (add-to-list 'eshell-visual-commands "lf")
    (add-to-list 'eshell-visual-commands "nvim")))

(use-package eshell-vterm
  :after eshell
  :config
  (eshell-vterm-mode))

(use-package eshell-z
  :hook (eshell-mode . (lambda () (require 'eshell-z))))

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'robbyrussell))

;; vertico
(use-package vertico
  :init
  (vertico-mode))

;; corfu
(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.

  :init
  (global-corfu-mode))

;; consult
(use-package consult :bind (("C-s" . consult-line)
                            ("C-x b" . consult-buffer)
                            ("M-g g" . consult-goto-line)))

;; elfeed
(use-package elfeed
  :config
  (setq elfeed-feeds
	'(("https://www.reddit.com/user/imunitic/m/tech/.rss" tech)
	  ("https://www.reddit.com/user/imunitic/m/games/.rss" games)))
  :bind
  ("C-c w" . elfeed))

(use-package ob-mermaid
  :config
  (setq ob-mermaid-cli-path "/opt/homebrew/bin/mmdc"))

;; (use-package iedit
;;   :bind (("C-;" . iedit-mode)))

;; (use-package multiple-cursors
;;   :bind (("C->" . mc/mark-next-like-this)
;;          ("C-<" . mc/mark-previous-like-this)
;;          ("C-c C-<" . mc/mark-all-like-this)))


;;; --- Evil core ------------------------------------------------------------
(use-package evil
  :init
  (setq evil-want-keybinding nil      ; needed for evil-collection
        evil-want-C-u-scroll nil      ; keep C-u as universal-argument
        evil-want-C-i-jump t
        evil-want-integration t
        evil-undo-system 'undo-redo
        evil-respect-visual-line-mode t)
  :config
  (evil-mode 1)
  ;; visual-line-aware j/k
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

;;; --- Evil collection (Magit, Dired, SLY, Help, etc.) ----------------------
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;;; --- Multiple cursors (Evil-native) ---------------------------------------
(use-package evil-mc
  :after evil
  :commands
  (evil-mc-make-all-cursors evil-mc-undo-all-cursors
   evil-mc-make-and-goto-next-match evil-mc-make-and-goto-prev-match
   evil-mc-make-cursor-in-visual-selection-end)
  :init
  (with-eval-after-load 'evil
    (global-evil-mc-mode 1)
    (define-key evil-normal-state-map (kbd "g m") #'evil-mc-make-all-cursors)
    (define-key evil-normal-state-map (kbd "g M") #'evil-mc-undo-all-cursors)
    (define-key evil-visual-state-map (kbd "g m") #'evil-mc-make-cursor-in-visual-selection-end)
    (define-key evil-normal-state-map (kbd "C-n") #'evil-mc-make-and-goto-next-match)
    (define-key evil-normal-state-map (kbd "C-p") #'evil-mc-make-and-goto-prev-match)))

;;; --- Iedit + Evil-friendly entry ------------------------------------------
(use-package iedit
  :bind (("C-;" . iedit-mode))
  :commands (iedit-mode iedit-restrict-function iedit-rectangle-mode))

(use-package evil-iedit-state
  :after (evil iedit)
  :commands (evil-iedit-state/iedit-mode)
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global (kbd "g r") #'evil-iedit-state/iedit-mode)
    (evil-define-key 'visual 'global (kbd "g r") #'evil-iedit-state/iedit-mode)))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

;; dumb-jump as the xref backend (straight.el + use-package)
(use-package dumb-jump
  :init
  ;; Always put dumb-jump first in this buffer's xref backend list
  (defun my/xref-prefer-dumb-jump ()
    (setq-local xref-backend-functions
                (cons #'dumb-jump-xref-activate
                      (remove #'dumb-jump-xref-activate xref-backend-functions))))
  ;; Ensure it wins both in plain prog-mode and when Eglot attaches
  (add-hook 'prog-mode-hook #'my/xref-prefer-dumb-jump)
  (with-eval-after-load 'eglot
    (add-hook 'eglot-managed-mode-hook #'my/xref-prefer-dumb-jump))

  :custom
  (dumb-jump-prefer-searcher 'rg)   ;; fastest with ripgrep
  ;; (dumb-jump-force-searcher 'rg) ;; uncomment to skip auto-detect
  (dumb-jump-aggressive t)

  :bind
  (("C-M-." . dumb-jump-go)
   ("C-M-," . dumb-jump-back)
   ("C-M-?" . dumb-jump-quick-look)))

;; --- Idle highlight of symbol at point (per-buffer minor mode) ---
(use-package idle-highlight-mode
  :commands (idle-highlight-mode)
  :init
  ;; Enable in code buffers; skip huge buffers for performance
  (defun my/enable-idle-highlight-maybe ()
    (when (and (derived-mode-p 'prog-mode 'conf-mode)
               (< (buffer-size) 500000))
      (idle-highlight-mode 1)))
  (add-hook 'after-change-major-mode-hook #'my/enable-idle-highlight-maybe)
  :custom
  (idle-highlight-idle-time 0.25)     ;; delay before highlighting
  (idle-highlight-inhibit-commands '(keyboard-quit)) ; don’t flash after quit
)

;; Tip: ensure ripgrep is installed so dumb-jump is fast:
;;   macOS (brew):  brew install ripgrep
;;   Linux (Debian): sudo apt-get install ripgrep


;; Other emacs configurations
;; Core Emacs tweaks (built-in)
(use-package emacs
  :preface
  ;; Window resize helpers for Shift+Meta H J K L
  (defun my/window-shrink-h () (interactive) (enlarge-window -1 t))
  (defun my/window-enlarge-v () (interactive) (enlarge-window  1))
  (defun my/window-shrink-v () (interactive) (enlarge-window -1))
  (defun my/window-enlarge-h () (interactive) (enlarge-window  1 t))

  :bind (("M-h" . windmove-left)
         ("M-j" . windmove-down)
         ("M-k" . windmove-up)
         ("M-l" . windmove-right)
         ("M-H" . my/window-shrink-h)
         ("M-J" . my/window-enlarge-v)
         ("M-K" . my/window-shrink-v)
         ("M-L" . my/window-enlarge-h))

  :hook ((prog-mode . completion-preview-mode)
         (lisp-mode . completion-preview-mode)
         (prog-mode . display-line-numbers-mode))

  :init
  ;; UI/behavior
  (setq-default cursor-type 'bar
                frame-title-format '("%b"))
  (setq inhibit-startup-screen t
        initial-scratch-message ""
        initial-buffer-choice t ;; land in *scratch*
        ring-bell-function 'ignore
        display-line-numbers-width 4
        display-line-numbers-type 'relative)

  (setq insert-directory-program "gls")
  (setq dired-use-ls-dired t)

  ;; Start Emacs fullscreen (choose one)
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  ;; or
  ;; (add-to-list 'initial-frame-alist '(fullscreen . fullboth))

  ;; y-or-n instead of yes-or-no
  (fset 'yes-or-no-p 'y-or-n-p)

  :config
  ;; Modes
  (show-paren-mode 1)
  (delete-selection-mode 1)
  (global-auto-revert-mode 1)

  ;; Font (guarded for daemon/availability)
  (defun my/set-preferred-font (&rest _)
    (when (member "Hurmit Nerd Font Mono" (font-family-list))
      (set-frame-font "Hurmit Nerd Font Mono 16" nil t)))
  (my/set-preferred-font)
  (add-hook 'after-make-frame-functions #'my/set-preferred-font)

  ;; Dired: allow visiting in same buffer with `a`
  (put 'dired-find-alternate-file 'disabled nil))


(provide 'base)
