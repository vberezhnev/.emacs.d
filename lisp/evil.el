;;; evil.el --- Evil mode configuration with leader key bindings

;; Enable dired-find-alternate-file (unchanged, but moved to dired-mode-hook for clarity)
(put 'dired-find-alternate-file 'disabled nil)

;; Dired setup with single-buffer navigation
(defun my-dired-setup ()
  "Set up dired for single-buffer navigation with Evil-like keybindings."
  ;; (local-set-key (kbd "RET") 'dired-find-alternate-file)
  (local-set-key (kbd "^") (lambda () (interactive) (dired-find-alternate-file "..")))
  (local-set-key (kbd "C-j") 'dired-next-line)
  (local-set-key (kbd "C-k") 'dired-previous-line)
  (local-set-key (kbd "C-h") 'dired-up-directory)
  (local-set-key (kbd "C-l") 'dired-find-alternate-file))

(add-hook 'dired-mode-hook #'my-dired-setup)
(setq dired-kill-when-opening-new-dired-buffer t)

;; Org-agenda keybindings
(defun my-org-agenda-keybindings ()
  "Set up Evil-like keybindings for org-agenda."
  (local-set-key (kbd "C-j") 'org-agenda-next-item)
  (local-set-key (kbd "C-k") 'org-agenda-previous-item)
  (local-set-key (kbd "C-h") 'org-agenda-earlier)
  (local-set-key (kbd "C-l") 'org-agenda-later))

(add-hook 'org-agenda-mode-hook #'my-org-agenda-keybindings)

;; Evil package: Load on demand
(use-package evil
  :straight t
  ;; :commands (evil-mode evil-normal-state)
  :init
  (setq evil-ex-complete-emacs-commands nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-shift-round nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-ex-set-initial-state 'normal
        evil-shift-width 2
        evil-want-keybinding nil) ;; Disable default keybindings for some modes
  :config
  (evil-mode 1)
  ;; Unbind arrow keys in motion, insert, and emacs states
  (dolist (map '(evil-motion-state-map evil-insert-state-map evil-emacs-state-map))
    (define-key (eval map) (kbd "<up>") nil)
    (define-key (eval map) (kbd "<down>") nil)
    (define-key (eval map) (kbd "<left>") nil)
    (define-key (eval map) (kbd "<right>") nil))
  ;; Dired keybindings in normal state
  (evil-define-key 'normal dired-mode-map
    (kbd "RET") 'dired-find-alternate-file
    (kbd "^") (lambda () (interactive) (dired-find-alternate-file ".."))
    (kbd "C-j") 'dired-next-line
    (kbd "C-k") 'dired-previous-line
    (kbd "C-h") 'dired-up-directory
    (kbd "C-l") 'dired-find-alternate-file))

;; Evil-leader: Load after evil
;; (use-package evil-leader
;;   :straight t
;;   ;; :after evil
;;   ;; :commands (global-evil-leader-mode evil-leader/set-leader)
;;   :config
;;   (global-evil-leader-mode)
;;   (evil-leader/set-leader "<SPC>"))

;; General: Load after evil-leader for leader key bindings
(use-package general
  :straight t
  ;; :after evil-leader
  :config
  (general-define-key
   :states '(normal visual insert emacs motion)
   "C-x l" 'evil-switch-to-windows-last-buffer)
  ;; Define leader key definer
  (general-create-definer my-leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "<SPC>")
  ;; Leader key bindings
  (my-leader-def
   "a" '(:ignore t :which-key "apps")
   "aa" 'org-agenda
   "ak" 'calculator
   "at" 'telega
   "al" 'leetcode
   "au" 'calendar
   "ae" 'eww
   "ar" 'elfeed
   "aw" '(lambda () (interactive) (wttrin "Vladivostok"))
   ;;games
   "ac" 'chess
   "ag" 'tetris

   "r" '(:ignore t :which-key "rust/cargo")
   ;; основные cargo команды
   "rr" 'rustic-cargo-run
   "rt" 'rustic-cargo-test
   "rb" 'rustic-cargo-build
   "rc" 'rustic-cargo-check
   "rl" 'rustic-cargo-clippy
   "rC" 'rustic-cargo-clean
   "rf" 'rustic-format-buffer
   "ra" 'rustic-cargo-add
   ;; расширенные команды
   "ru" 'rustic-cargo-update
   "rx" 'rustic-cargo-uninstall
   "rd" 'rustic-cargo-doc
   "rB" 'rustic-cargo-bench
   "rF" 'rustic-cargo-fix
   
   "l" '(:ignore t :which-key "LSP")
   "lf" 'consult-flymake
   "lr" 'replace-string
  
   "p" '(:ignore t :which-key "projectile")
   "pf" 'projectile-find-file
   "pd" 'projectile-find-dir
   "pp" 'projectile-switch-project
   "pa" 'projectile-add-known-project
   "ps" 'projectile-ripgrep

   "f" '(:ignore t :which-key "files")
   "fr" 'consult-recent-file
   "fs" 'save-buffer
   "ff" 'ace-link

   "g" '(:ignore t :which-key "git")
   "gg" 'magit
   "gc" 'magit-clone

   "e" '(:ignore t :which-key "eval+bookmarks")
   "ee" 'eval-buffer
   "er" 'eval-region
   ;; "e" '(:ignore t :which-key "bookmarks")
   "eb" 'consult-bookmark
   "em" 'bookmark-set
   "el" 'bookmark-bmenu-list


   "o" '(:ignore t :which-key "org")
   "oc" 'org-capture
   "ol" 'org-store-link
   "of" 'org-footnote-new
   "op" 'my/org-pomodoro
   "ot" 'org-timeblock
   "ob" 'org-cite-insert
   "odm" 'org-download-clipboard
   "odo" 'org-download-image
   "odi" 'org-cliplink

   "d" '(:ignore t :which-key "org-gtd")
   "dc" '(lambda () (interactive) (org-gtd-capture nil "i"))
   "de" 'org-gtd-engage
   "dp" 'org-gtd-process-inbox
   "dn" 'org-gtd-show-all-next
   "ds" 'org-gtd-review-stuck-projects

   "n" '(:ignore t :which-key "notes")
   "nl" 'org-roam-buffer-toggle
   "nf" 'org-roam-node-find
   "ni" 'org-roam-node-insert
   "nc" 'org-roam-capture
   "nt" 'org-roam-tag-add
   "nr" 'org-roam-ref-add
   "nj" 'org-roam-dailies-capture-today

   "s" '(:ignore t :which-key "ai")
   "sw" 'whisper-run
   "ss" 'gptel
   "sd" 'gptel-context-add
   "sf" 'gptel-add-file
   "sa" 'gptel-menu
   "sr" 'gptel--regenerate
   "se" 'gptel-rewrite
   "sq" 'gptel-quick

   "m" '(:ignore t :which-key "misc")
   "ms" 'scratch-buffer
   "my" 'hydra-main/body

   "t" '(:ignore t :which-key "treemacs")
   "tt" 'org-timer-set-timer
   "ts" 'org-timer-start
   "tc" 'org-timer-stop
   "tp" 'org-timer-pause-or-continue
   ;; "tt" 'treemacs
   ;; "td" 'treemacs-select-directory
   ;; "tb" 'treemacs-bookmark

   "b" 'bluetooth-list-devices
   "w" 'multi-vterm
   "k" 'kill-buffer
   "q" 'rzgrep
   "h" 'global-hide-mode-line-mode)
  ;; Mode-specific bindings
  (general-def :states 'normal :keymaps 'elfeed-search-mode-map
    "<return>" 'elfeed-search-show-entry
    "r" 'elfeed-search-update--force
    "u" 'elfeed-unjam
    "af" 'elfeed-tube-fetch
    "as" 'elfeed-tube-save)
  (general-def :states '(normal motion) :keymaps 'elfeed-show-mode-map
    "o" 'elfeed-show-visit)
  (general-def :states 'normal :keymaps 'org-mode-map
    "i" 'org-cite-insert)
  (general-def :states 'normal :keymaps 'magit-mode-map
    "SPC" nil) ;; Unbind SPC to avoid leader key conflicts
  (general-def :states 'normal :keymaps 'dired-mode-map
    "." 'dired-omit-mode
    "h" 'dired-up-directory
    "l" 'dired-find-file
    "s" 'save-buffer)
  (general-def :states 'normal
    "s" 'save-buffer
    "q" 'kill-current-buffer
    "u" 'undo-tree-undo
    "r" 'undo-tree-redo))

;; Evil-collection: Load after evil for mode-specific bindings
(use-package evil-collection
  :straight t
  :after evil
  ;; :commands (evil-collection-init)
  :config
  (setq evil-want-integration t)
  (evil-collection-init '(ibuffer bookmark vterm magit xwidget-webkit sunrise))
  ;; (evil-set-initial-state 'elfeed-show-mode 'emacs)
  (evil-set-initial-state 'ibuffer-mode 'normal)
  (evil-set-initial-state 'bookmark-bmenu-mode 'normal)
  (evil-set-initial-state 'vterm-mode 'normal)
  (evil-set-initial-state 'magit-mode 'emacs)
  
  (evil-set-initial-state 'telega-root-mode 'normal)
  (evil-set-initial-state 'telega-chat-mode 'normal)

  (evil-set-initial-state 'eww-mode 'normal)
  (evil-set-initial-state 'bluetooth-mode 'emacs)
  (evil-set-initial-state 'chess-pgn-mode 'emacs)

  (evil-set-initial-state 'org-super-agenda-mode 'emacs)
  (evil-set-initial-state 'calculator-mode 'emacs)
  (evil-set-initial-state 'calendar-mode 'emacs)
  (evil-set-initial-state 'enlight-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (evil-set-initial-state 'org-timeblock-mode 'emacs)
  (evil-set-initial-state 'org-timeblock-list-mode 'emacs)

  (evil-set-initial-state 'treemacs-mode 'normal)
  (evil-set-initial-state 'xwidget-webkit-mode 'emacs)
  (evil-set-initial-state 'sunrise-mode 'emacs))

(use-package evil-anzu
  :straight t
  :config
  (global-anzu-mode))

;; Evil-terminal-cursor-changer: Load only in terminal
(use-package evil-terminal-cursor-changer
  :straight t
  :unless (display-graphic-p)
  :commands (evil-terminal-cursor-changer-activate)
  :config
  (evil-terminal-cursor-changer-activate)
  (setq evil-motion-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-emacs-state-cursor 'hbar))

;; Undo-tree: Load on demand for undo/redo commands
(use-package undo-tree
  :straight t
  ;; :commands (global-undo-tree-mode undo-tree-undo undo-tree-redo)
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

;; Which-key: Load on demand for keybinding hints
(use-package which-key
  :straight t
  :commands (which-key-mode)
  :config
  (which-key-mode 1))

;; Which-key-posframe: Load after which-key
(use-package which-key-posframe
  :straight t
  :after which-key
  ;; :commands (which-key-posframe-mode)
  :config
  (which-key-posframe-mode)
  (setq which-key-idle-delay 0.3))

;; Char-fold: Load on demand for search functionality
(use-package char-fold
  :straight t
  ;; :commands (char-fold-to-regexp)
  :custom
  (char-fold-symmetric t)
  (search-default-mode #'char-fold-to-regexp))

;; Reverse-im: Load after char-fold
(use-package reverse-im
  :straight t
  ;; :after char-fold
  ;; :commands (reverse-im-mode reverse-im-translate-word)
  :demand t
  :bind ("M-T" . reverse-im-translate-word)
  :custom
  (reverse-im-cache-file (locate-user-emacs-file "reverse-im-cache.el"))
  (reverse-im-char-fold t)
  (reverse-im-read-char-advice-function #'reverse-im-read-char-include)
  (reverse-im-input-methods '("ukrainian-computer"))
  :config
  (reverse-im-mode t))

(provide 'evil)
