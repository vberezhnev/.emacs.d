(setq evil-want-keybinding nil)

;; (use-package xah-fly-keys
;; 	:ensure nil)

(use-package general
	:ensure t
	:config
	(general-define-key
	 :states '(normal visual insert emacs motion)
	 "C-x l" 'evil-switch-to-windows-last-buffer
	 "C-x SPC" 'hydra-main/body
	 "C-x ;"  'my/smart-buffers-list
	 "C-x , ;" 'consult-buffer))

(use-package evil-leader
  :ensure t
	:after evil
  :config
  (evil-leader/set-leader "SPC")
	(evil-leader/set-key
		"w" 'evil-window-vsplit)

	;; (evil-leader/set-key
	;; "de" 'org-gtd-engage
	;; "dr" 'org-gtd-engage-grouped-by-context
	;; "dp" 'org-gtd-process-inbox
	;; "c" 'org-gtd-organize)

	(evil-leader/set-key
		"z" '(org-agenda nil "z")
		"qs" 'hq-shop
		"qb" 'hq-buy-item
		"qu" 'hq-use-item)

	(evil-leader/set-key
		;; "dc" 'org-gtd-capture
		"dc" (lambda () (interactive) (org-gtd-capture nil "i"))
		"de" 'org-gtd-engage
		"dp" 'org-gtd-process-inbox
		"dn" 'org-gtd-show-all-next
		"ds" 'org-gtd-review-stuck-projects)

  (global-evil-leader-mode))

(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  ;; (evil-select-search-module 'evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  ;; (setq evil-vsplit-window-right t)
  ;; (setq evil-split-window-below t)
  ;;(setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)

  (setq evil-ex-set-initial-state 'normal)

  :config ;; tweak evil after loading it
  (evil-mode))

;; (eval-after-load "evil-maps"
;;   (dolist (map '(evil-motion-state-map
;;                  evil-insert-state-map
;;                  evil-emacs-state-map))
;;     (define-key (eval map) (kbd "<up>") nil)
;;     (define-key (eval map) (kbd "<down>") nil)
;;     (define-key (eval map) (kbd "<left>") nil)
;;     (define-key (eval map) (kbd "<right>") nil)))

(use-package evil-terminal-cursor-changer
	:ensure t
	:config
	(unless (display-graphic-p)
    (require 'evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate))
	(setq evil-motion-state-cursor 'box)  ; █
  (setq evil-visual-state-cursor 'box)  ; █
  (setq evil-normal-state-cursor 'box)  ; █
  (setq evil-insert-state-cursor 'bar)  ; ⎸
  (setq evil-emacs-state-cursor  'hbar)) ; _

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode))

;; ;; @FIXME missing config
;; (use-package evil-indent-textobject
;;   :ensure t)

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

;; (use-package evil-set-option
;;   :ensure t
;;   :quelpa (evil-set-option
;;            :fetcher github
;;            :repo "FrostyX/evil-set-option"
;;            :branch "main")
;;   :config
;;   (evil-set-option-mode))

(use-package evil-collection
  :ensure t
  :after evil
  :config
	(setq evil-want-integration t)
  (evil-set-initial-state 'ibuffer-mode 'normal)
  (evil-set-initial-state 'bookmark-bmenu-mode 'normal)
  (evil-set-initial-state 'vterm-mode 'normal)
	(evil-set-initial-state 'magit-mode 'emacs)
  (evil-set-initial-state 'org-timeblock-mode 'emacs)
  (evil-set-initial-state 'org-agenda-mode 'emacs)
  (evil-set-initial-state 'org-super-agenda-mode 'emacs)
  (evil-set-initial-state 'calibredb-mode 'normal)
  (evil-set-initial-state 'enlight-mode 'emacs)
  (evil-set-initial-state 'org-timeblock-mode 'emacs)
  (evil-set-initial-state 'org-timeblock-list-mode 'emacs)
  ;; (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'treemacs-mode 'emacs)
  (evil-set-initial-state 'xwidget-webkit-mode 'emacs)
  (evil-set-initial-state 'sunrise-mode 'emacs)
  (evil-collection-init))

(use-package reverse-im
  ;; :ensure t ; install `reverse-im' using package.el
  :quelpa (reverse-im
           :fetcher github
           :repo "emacsmirror/reverse-im")
  :demand t
  ;;:after char-fold ; but only after `char-fold' is loaded
  :custom
  (reverse-im-cache-file (locate-user-emacs-file "reverse-im-cache.el"))
  (reverse-im-char-fold t)
  (reverse-im-read-char-advice-function #'reverse-im-read-char-include)
  (reverse-im-input-methods '("ukrainian-computer"))
  :config
  (reverse-im-mode t)) ; turn the mode on

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

(provide 'evil)
