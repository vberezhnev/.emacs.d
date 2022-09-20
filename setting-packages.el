;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Setting packages ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pdf-tools
  :defer t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  :bind (:map pdf-view-mode-map
							("\\" . hydra-pdftools/body)
							("<s-spc>" .  pdf-view-scroll-down-or-next-page)
							("g"  . pdf-view-first-page)
							("G"  . pdf-view-last-page)
							("l"  . image-forward-hscroll)
							("h"  . image-backward-hscroll)
							("j"  . pdf-view-next-page)
							("k"  . pdf-view-previous-page)
							("e"  . pdf-view-goto-page)
							("u"  . pdf-view-revert-buffer)
							("al" . pdf-annot-list-annotations)
							("ad" . pdf-annot-delete)
							("aa" . pdf-annot-attachment-dired)
							("am" . pdf-annot-add-markup-annotation)
							("at" . pdf-annot-add-text-annotation)
							("y"  . pdf-view-kill-ring-save)
							("i"  . pdf-misc-display-metadata)
							("s"  . pdf-occur)
							("b"  . pdf-view-set-slice-from-bounding-box)
							("r"  . pdf-view-reset-slice)))

(defhydra hydra-pdftools (:color blue :hint nil)
  "
                                                                     ╭───────────┐
      Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
  ╭──────────────────────────────────────────────────────────────────┴───────────╯
        ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
        ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
        ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
        ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
   _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
        ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
        ^^_n_^^      ^ ^  _r_eset slice box
        ^^^↓^^^
        ^^_G_^^
  --------------------------------------------------------------------------------
       "
  ("\\" hydra-master/body "back")
  ("<ESC>" nil "quit")
  ("al" pdf-annot-list-annotations)
  ("ad" pdf-annot-delete)
  ("aa" pdf-annot-attachment-dired)
  ("am" pdf-annot-add-markup-annotation)
  ("at" pdf-annot-add-text-annotation)
  ("y"  pdf-view-kill-ring-save)
  ("+" pdf-view-enlarge :color red)
  ("-" pdf-view-shrink :color red)
  ("0" pdf-view-scale-reset)
  ("H" pdf-view-fit-height-to-window)
  ("W" pdf-view-fit-width-to-window)
  ("P" pdf-view-fit-page-to-window)
  ("n" pdf-view-next-page-command :color red)
  ("p" pdf-view-previous-page-command :color red)
  ("d" pdf-view-dark-minor-mode)
  ("b" pdf-view-set-slice-from-bounding-box)
  ("r" pdf-view-reset-slice)
  ("g" pdf-view-first-page)
  ("G" pdf-view-last-page)
  ("e" pdf-view-goto-page)
  ("o" pdf-outline)
  ("s" pdf-occur)
  ("i" pdf-misc-display-metadata)
  ("u" pdf-view-revert-buffer)
  ("F" pdf-links-action-perfom)
  ("f" pdf-links-isearch-link)
  ("B" pdf-history-backward :color red)
  ("N" pdf-history-forward :color red)
  ("l" image-forward-hscroll :color red)
  ("h" image-backward-hscroll :color red))

(use-package elcord
  :hook (after-init . elcord-mode))

(use-package org-superstar
	:init
	(setq org-superstar-mode t)
	:config
	(setq org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿")))

(use-package multi-term
  :bind
  ("C-x q" . multi-term-dedicated-toggle) ;; Open multi-term quickly
  ("C-x w" . multi-term)) ;; Open default multi-term without automate spliting

(use-package evil
  :ensure t
  :init      ;; tweak evil's configuration before loading it
  ;;(setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode))
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))
(use-package evil-tutor
  :ensure t)

(use-package fzf
	:ensure t
  :bind
    ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

;; (use-package general
;;   :config
;; 	:ensure t
;;   (general-evil-setup t))

(add-hook 'web-mode-hook 'prettier-js-mode)

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
					(funcall (cdr my-pair)))))

(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
															'("\\.jsx?\\'" . prettier-js-mode))))

(use-package json-mode :ensure t :defer 20
  :custom
  (json-reformat:indent-width 2)
  :mode (("\\.bowerrc$"     . json-mode)
         ("\\.jshintrc$"    . json-mode)
         ("\\.json_schema$" . json-mode)
         ("\\.json\\'" . json-mode))
  :bind (:package json-mode-map
									:map json-mode-map
									("C-c <tab>" . json-mode-beautify)))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               nil
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;; (treemacs-resize-icons 48)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-all-the-icons
  :ensure t)
(treemacs-load-theme "all-the-icons")

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package format-all
  :ensure t
  :preface
  (defun ian/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (if (derived-mode-p 'prolog-mode)
        (prolog-indent-buffer)
      (format-all-buffer)))
  :config
  (global-set-key (kbd "M-F") #'ian/format-code)
  (add-hook 'prog-mode-hook #'format-all-ensure-formatter))


;; (use-package undo-fu
;; 	:config
;;   (global-unset-key (kbd "C-z"))
;;   (global-set-key (kbd "C-z")   'undo-fu-only-undo)
;;   (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))


(use-package centaur-tabs
  ;; :load-path "~/.emacs.d/other/centaur-tabs"
  :config
  (setq centaur-tabs-style "bar"
				centaur-tabs-height 32
				centaur-tabs-set-icons t
				centaur-tabs-set-modified-marker t
				;; centaur-tabs-show-navigation-buttons t
				centaur-tabs-set-bar 'over
				;; x-underline-at-descent-line t
				)
  (centaur-tabs-headline-match)
  ;; (setq centaur-tabs-gray-out-icons 'buffer)
  ;; (centaur-tabs-enable-buffer-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
			;; ((not (eq (file-remote-p (buffer-file-name)) nil))
			;; "Remote")
			((or (string-equal "*" (substring (buffer-name) 0 1))
					 (memq major-mode '(magit-process-mode
															magit-status-mode
															magit-diff-mode
															magit-log-mode
															magit-file-mode
															magit-blob-mode
															magit-blame-mode
															)))
			 "Emacs")
			((derived-mode-p 'prog-mode)
			 "Editing")
			((derived-mode-p 'dired-mode)
			 "Dired")
			((memq major-mode '(helpful-mode
													help-mode))
			 "Help")
			((memq major-mode '(org-mode
													org-agenda-clockreport-mode
													org-src-mode
													org-agenda-mode
													org-beamer-mode
													org-indent-mode
													org-bullets-mode
													org-cdlatex-mode
													org-agenda-log-mode
													diary-mode))
			 "OrgMode")
			(t
			 (centaur-tabs-get-group-name (current-buffer))))))
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-c t s" . centaur-tabs-counsel-switch-group)
  ("C-c t p" . centaur-tabs-group-by-projectile-project)
  ("C-c t g" . centaur-tabs-group-buffer-groups)
  (:map evil-normal-state-map
				("g t" . centaur-tabs-forward)
				("g T" . centaur-tabs-backward)))

(use-package moe-theme
  :ensure t)

(use-package melancholy-theme
  :ensure t)

(use-package doom-modeline
  :commands doom-modeline
  ;;:config
  ;;(setq doom-modeline-height 15)
  ;;(setq doom-modeline-bar-width 3)
  ;;(setq doom-modeline-minor-modes (featurep 'minions))
  ;;(setq doom-modeline-buffer-file-name-style 'buffer-name)
  ;;(doom-modeline-set-timemachine-modeline)
  ;;(doom-modeline-time t)

  :custom
  (doom-modeline-set-timemachine-modeline)
	(doom-modeline-buffer-name t)
	(display-battery-mode 1)
	(doom-modeline-time-icon t)
	;; (doom-modeline-height 15)
  (doom-modeline-bar-width 3)
  :hook (after-init . doom-modeline-mode))

(use-package parrot
  :hook (after-init . parrot-mode))

;; Define your custom doom-modeline
;;(doom-modeline-def-modeline 'my-simple-line
;;  '(bar matches buffer-info remote-host buffer-position selection-info parrot nyan)
;;  '(misc-info minor-modes input-method buffer-encoding major-mode process vcs checker))

;;(use-package term
;;  :commands term
;;  :config
;;  (setq explicit-shell-file-name "fish") ;; Change this to zsh, etc
;;  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args
;;
;;  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
;;  ;;(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package company :ensure t :defer 20
  ;; This is not perfect yet. It completes too quickly outside programming modes, but while programming it is just right.
  :custom
  (company-idle-delay 0.1)
  (global-company-mode t)
  (debug-on-error nil) ;; otherwise this throws lots of errors on completion errors
  :config
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map [return] 'company-complete-selection)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  ;; auto-complete compatibility
  (defun my-company-visible-and-explicit-action-p ()
    (and (company-tooltip-visible-p)
				 (company-explicit-action-p)))
  (defun company-ac-setup ()
    "Sets up `company-mode' to behave similarly to `auto-complete-mode'."
    (setq company-require-match nil)
    (setq company-auto-complete #'my-company-visible-and-explicit-action-p)
    (setq company-frontends '(company-echo-metadata-frontend
															company-pseudo-tooltip-unless-just-one-frontend-with-delay
															company-preview-frontend))
    (define-key company-active-map [tab]
			'company-select-next-if-tooltip-visible-or-complete-selection)
    (define-key company-active-map (kbd "TAB")
			'company-select-next-if-tooltip-visible-or-complete-selection))

  (company-ac-setup)
  (add-hook 'js2-mode-hook (lambda () (company-mode))))

;; (use-package all-the-icons
;;   :if (display-graphic-p))
;; 	:config
;;   ;; Make sure the icon fonts are good to go
;;   (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
;;   (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
;;   (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
;;   (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
;;   (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
;;   (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append))

;; TODO: Add a function to set window width to fill column width
;; according to current major mode
(use-package zoom
  :ensure t
  :commands zoom-mode
  :preface
  (defvar fk/zoom-default-size '(120 . 40))
  :custom
  (zoom-size fk/zoom-default-size)
  :bind*
  (("C-M-=" . fk/enlarge-window)
   ("C-M--" . fk/shrink-window)
   ("C-M-0" . balance-windows))
  :config
  ;; TODO: handle when zoom-mode active
  (defun fk/adjust-window-width (percentage)
    (if (and olivetti-mode (= (count-windows) 1))
				(if (> percentage 1.0) (olivetti-expand) (olivetti-shrink))
			(let* ((new-width (round (* (window-width) percentage)))
						 (zoom-size (cons new-width (cdr zoom-size))))
				(if (> percentage 1.0)  ; TODO: fk/smooth-zoom do not shrink
						(fk/smooth-zoom)
					(zoom)))))

  (defun fk/enlarge-window ()
    (interactive)
    (fk/adjust-window-width 1.1))

  (defun fk/shrink-window ()
    (interactive)
    (fk/adjust-window-width 0.9))

  (defvar fk/smooth-zoom-steps 10)
  (defvar fk/smooth-zoom-period 0.01)

  (defun fk/floor (number)
    "Floor by absolute value."
    (if (< number 0)
				(ceiling number)
			(floor number)))

  (defun fk/smooth-zoom ()
    "Smooth (animated) version of `zoom'."
    (interactive)
    (cancel-function-timers 'fk/smooth-zoom--resize)
    (setq fk/smooth-zoom-sizes '())
    (setq fk/smooth-zoom-window (get-buffer-window))
    (let* ((current-size (cons (window-width) (window-height)))
					 (desired-size zoom-size)
					 (distances (cons (- (car desired-size) (car current-size))
														(- (cdr desired-size) (cdr current-size))))
					 (step-distance (cons (fk/floor (/ (car distances) (float fk/smooth-zoom-steps)))
																(fk/floor (/ (cdr distances) (float fk/smooth-zoom-steps))))))
			(dotimes (i fk/smooth-zoom-steps)
				(let* ((zoom-size (if (< i (1- fk/smooth-zoom-steps))
															(cons (+ (car step-distance) (car current-size))
																		(+ (cdr step-distance) (cdr current-size)))
														desired-size))
							 (time (concat (number-to-string (round (* i fk/smooth-zoom-period 1000))) " millisec")))
					(setq current-size zoom-size)
					(add-to-list 'fk/smooth-zoom-sizes current-size t)
					(run-at-time time nil 'fk/smooth-zoom--resize)))))

  (defun fk/smooth-zoom--resize ()
    (with-selected-window fk/smooth-zoom-window
			(let ((zoom-size (pop fk/smooth-zoom-sizes)))
				(zoom--resize)))))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
							("s-p" . projectile-command-map)
							("C-c p" . projectile-command-map)))

;; Highlight TODO, FIXME, ... in any programming mode
;; (use-package 'fic-mode
;; 	:ensure t
;; 	:hook 'prog-mode-hook 'fic-mode)

;; Setting dashboard
(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-center-content t)
																				;(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;;(setq dashboard-startup-banner "~/Изображения/Logos/dailyminimal/Olivia Black.jpeg")  ;; use custom image as banner
  (setq dashboard-startup-banner "~/.emacs.d/images/RMS.png")  ;; use custom image as banner
  (setq dashboard-items '(
													(recents . 5)
													;;(agenda . 5 )
													(bookmarks . 3)
													(projects . 5)))
	;;(registers . 3)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
  																	(bookmarks . "book"))))
