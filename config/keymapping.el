(use-package evil
  ;; :defer nil
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode))

(use-package general
  :after evil
  :config
  (general-evil-setup t))

(use-package evil-collection
  :defer nil
  :after evil
  :config
  (setq evil-emacs-state-cursor '("#FF5D62" box))
  (setq evil-normal-state-cursor '("#FF5D62" box))
  (setq evil-visual-state-cursor '("#98BB6C" box))
  (setq evil-insert-state-cursor '("#E82424" bar))
  (setq evil-replace-state-cursor '("#FF9E3B" hbar))
  (setq evil-operator-state-cursor '("#7E9CD8" hollow))
	(evil-set-initial-state 'ibuffer-mode 'normal)
	(evil-set-initial-state 'bookmark-bmenu-mode 'normal)
	(evil-set-initial-state 'vterm-mode 'normal)
	(evil-set-initial-state 'calibredb-mode 'normal)
	;; (evil-set-initial-state 'dired-mode 'emacs)
	(evil-set-initial-state 'sunrise-mode 'emacs)
  (evil-collection-init))

(use-package fzf
  :bind
  ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll "
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))
(defun fzf-node-project ()
  (interactive)
  (let ((process-environment
         (cons (concat "FZF_DEFAULT_COMMAND=ag -g \"\" --ignore node_modules .git build dist")
               process-environment)))
    (fzf/start default-directory)))
