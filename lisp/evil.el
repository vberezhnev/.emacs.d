;;; evil.el --- Evil mode configuration with leader key bindings

;; Disable default Evil keybindings for some modes
(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :init
  (setq evil-ex-complete-emacs-commands nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-shift-round nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-ex-set-initial-state 'normal
        evil-shift-width 2)
  :config
  (evil-mode 1)
  ;; Unbind arrow keys in motion, insert, and emacs states
  (dolist (map '(evil-motion-state-map evil-insert-state-map evil-emacs-state-map))
    (define-key (eval map) (kbd "<up>") nil)
    (define-key (eval map) (kbd "<down>") nil)
    (define-key (eval map) (kbd "<left>") nil)
    (define-key (eval map) (kbd "<right>") nil))
  ;; Unbind common Ctrl keys globally in normal state
  (general-def :states 'normal
    "C-x" nil
    "C-c" nil))

(use-package evil-leader
  :ensure t
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

(use-package general
  :ensure t
  :after evil-leader
  :config
	(general-define-key
   :states '(normal visual insert emacs motion)
   "C-x l" 'evil-switch-to-windows-last-buffer)
	
  ;; Define a leader key definer
  (general-create-definer my-leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "<SPC>")
  
  ;; General keybindings under leader
  (my-leader-def
    "a" '(:ignore t :which-key "apps")
    "aa" 'org-agenda
    "cx" 'elfeed
    
    ;; Window management
    ;; "w" '(:ignore t :which-key "windows")
    ;; "wv" 'evil-window-vsplit
    ;; "ws" 'evil-window-split
    ;; "wd" 'evil-window-delete
    ;; "wh" 'evil-window-left
    ;; "wj" 'evil-window-down
    ;; "wk" 'evil-window-up
    ;; "wl" 'evil-window-right
    
    ;; File operations
    "f" '(:ignore t :which-key "files")
    "ff" 'helm-find-files
    "fr" 'helm-recentf
    "fs" 'save-buffer
    
    ;; Git/Magit
    "g" '(:ignore t :which-key "git")
    "gg" 'magit
    "gc" 'magit-clone

		;; Eval
    "e" '(:ignore t :which-key "eval")
    "ee" 'eval-buffer
    "er" 'eval-region
		
    ;; Org-mode
    "o" '(:ignore t :which-key "org")
    "oc" 'org-capture
    "ol" 'org-store-link
    "of" 'org-footnote-new
    "op" 'my/org-pomodoro
    "ot" 'org-timeblock
    "og" 'org-gtd-capture
    "oe" 'org-gtd-engage
    "oi" 'org-gtd-process-inbox
    "on" 'org-gtd-show-all-next
    "os" 'org-gtd-review-stuck-projects
    "ob" 'org-cite-insert
    "odm" 'org-download-clipboard
    "odo" 'org-download-image
    "odi" 'org-cliplink
    
    ;; Org-roam
    "n" '(:ignore t :which-key "notes")
    "nl" 'org-roam-buffer-toggle
    "nf" 'org-roam-node-find
    "ni" 'org-roam-node-insert
    "nc" 'org-roam-capture
    "nt" 'org-roam-tag-add
    "nr" 'org-roam-ref-add
    "nj" 'org-roam-dailies-capture-today
    
    ;; AI/GPTel
    "s" '(:ignore t :which-key "ai")
    "ss" 'gptel
    "sd" 'gptel-context-add
    "sf" 'gptel-add-file
    "sa" 'gptel-menu
    "sr" 'gptel--regenerate
    "se" 'gptel-rewrite
    
    ;; Misc
    "m" '(:ignore t :which-key "misc")
    "ms" 'scratch-buffer
    "my" 'hydra-main/body
    
    ;; Toggle modes
    "t" '(:ignore t :which-key "toggle")
    "tt" 'treemacs
    "td" 'treemacs-select-directory
    "tb" 'treemacs-bookmark
    
    ;; Top-level bindings
    "b" 'bluetooth-list-devices
    "q" 'multi-vterm
    "x" 'helm-M-x
    "h" 'global-hide-mode-line-mode)

  ;; Mode-specific bindings
  (general-def :states 'normal :keymaps 'elfeed-search-mode-map
    "<return>" 'elfeed-search-show-entry
    ;; "o" 'elfeed-search-browse-url
    "r" 'elfeed-search-update--force
    "u" 'elfeed-unjam
    "af" 'elfeed-tube-fetch
    "as" 'elfeed-tube-save)

  (general-def :states '(normal motion) :keymaps 'elfeed-show-mode-map
    "o" 'elfeed-show-visit
    ;; "j" 'elfeed-show-next
    ;; "k" 'elfeed-show-prev
    "f" 'elfeed-tube-fetch
    "s" 'elfeed-tube-save)

  (general-def :states 'normal :keymaps 'org-mode-map
    "i" 'org-cite-insert
    "dm" 'org-download-clipboard
    "do" 'org-download-image
    "di" 'org-cliplink)

  (general-def :states 'normal :keymaps 'magit-mode-map
    "SPC" nil) ; Unbind SPC to avoid leader key conflicts

  (general-def :states 'normal :keymaps 'dired-mode-map
    "." 'dired-omit-mode
    "h" 'dired-up-directory
    "l" 'dired-find-file
    "s" 'save-buffer)

  ;; Quick bindings in normal state (global)
  (general-def :states 'normal
    "s" 'save-buffer
    "q" 'kill-current-buffer
    "u" 'undo-tree-undo
    "r" 'undo-tree-redo))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init '(ibuffer bookmark vterm magit org-timeblock enlight xwidget-webkit sunrise helm))
  (evil-set-initial-state 'ibuffer-mode 'normal)
  (evil-set-initial-state 'bookmark-bmenu-mode 'normal)
  (evil-set-initial-state 'vterm-mode 'normal)
  (evil-set-initial-state 'magit-mode 'emacs)
  (evil-set-initial-state 'bluetooth-mode 'emacs)
  (evil-set-initial-state 'org-timeblock-mode 'emacs)
  ;; (evil-set-initial-state 'org-agenda-mode 'emacs)
  (evil-set-initial-state 'org-super-agenda-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)
  ;; (evil-set-initial-state 'telega-mode-line-mode 'emacs)
  ;; (evil-set-initial-state 'calibredb-mode 'normal)
  (evil-set-initial-state 'enlight-mode 'emacs)
  (evil-set-initial-state 'org-timeblock-list-mode 'emacs)
  (evil-set-initial-state 'treemacs-mode 'emacs)
  (evil-set-initial-state 'xwidget-webkit-mode 'emacs)
  (evil-set-initial-state 'sunrise-mode 'emacs))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode 1))

(use-package evil-terminal-cursor-changer
  :ensure t
  :config
  (unless (display-graphic-p)
    (evil-terminal-cursor-changer-activate))
  (setq evil-motion-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-emacs-state-cursor 'hbar))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.3))

(provide 'evil)
