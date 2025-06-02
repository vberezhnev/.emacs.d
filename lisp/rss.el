(use-package nano-theme
	:ensure t)

(defun my/centre-visual-fill-on ()
  ;; Centre the text in one column
  (visual-line-mode 1)
  (setq olivetti-body-width 100)
  (olivetti-mode 1))

(defun my/centre-visual-fill-off ()
  (visual-line-mode 0)
  (kill-local-variable 'olivetti-body-width)
  (olivetti-mode 0))

(defun my/kill-modeline ()
  (setq-local mode-line-format nil))

(defun my/restore-modeline ()
    (kill-local-variable 'mode-line-format))

(defun my/toggle-modeline ()
  (interactive)
  (if (null mode-line-format)
      (my/restore-modeline)
    (my/kill-modeline)))

(defun my/hide-cursor-evil ()
  (setq-local evil-default-cursor '(ignore))
  (setq-local cursor-type nil))

(defun my/show-cursor-evil ()
  (kill-local-variable 'evil-default-cursor)
  (kill-local-variable 'cursor-type))

(defun my/toggle-cursor ()
  (interactive)
  (if (null cursor-type)
      (my/show-cursor-evil)
    (my/hide-cursor-evil)))

(defun my/reading-mode-hook ()
  (my/centre-visual-fill-on)
  (my/hide-cursor-evil)
  (my/kill-modeline))

(defun my-view-down ()
  (interactive)
  (if (null cursor-type)
      (scroll-up-line)
    (next-line)))

(defun my-view-up ()
  (interactive)
  (if (null cursor-type)
      (scroll-down-line)
    (previous-line)))


(use-package elfeed
  :ensure t
  ;; :commands (elfeed)
  :bind ("C-c x" . elfeed) ; Global keybinding to launch elfeed
	:hook
	(elfeed-show-mode . my/reading-mode-hook)
  :config
	(evil-define-key 'normal elfeed-show-mode-map (kbd "j") 'my-view-down)
	(evil-define-key 'normal elfeed-show-mode-map (kbd "k") 'my-view-up)
	
  (setq-default elfeed-search-filter "")
	(setq elfeed-search-trailing-width 15)
	(defun elfeed-search-format-date (date) "")
	(setq elfeed-db-directory "~/.elfeed")

  ;; Keybindings for elfeed-search-mode
  (with-eval-after-load "elfeed-search"
    (evil-define-key* 'normal elfeed-search-mode-map
      "RET" #'elfeed-search-show-entry
      "o" #'elfeed-search-browse-url
      "r" #'elfeed-search-update--force ; Refresh articles in elfeed-search-mode
      "S" #'elfeed-unjam
      "F" #'elfeed-tube-fetch
      [remap save-buffer] #'elfeed-tube-save))
  ;; Keybindings for elfeed-show-mode
  ;; (with-eval-after-load "elfeed-show"
  ;;   (evil-define-key* 'motion elfeed-show-mode-map
  ;;     "gb" #'elfeed-show-visit
  ;;     "gj" #'elfeed-show-next
  ;;     "gk" #'elfeed-show-prev))
	)

(use-package stripes
	:load-path "~/.emacs.d/lisp/packages/nano-elfeed/")

(use-package relative-date
	:load-path "~/.emacs.d/lisp/packages/nano-elfeed/")

(use-package nano-elfeed
	:load-path "~/.emacs.d/lisp/packages/nano-elfeed/")

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
	(setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

(setq my/elfeed-update-timer
			(run-at-time nil (* 1 60 60 24) #'elfeed-update))

;; (with-eval-after-load "elfeed-search"
;;   (evil-define-key*
;;     'normal elfeed-search-mode-map
;;     "RET" #'elfeed-search-show-entry
;;     "o" #'elfeed-search-browse-url
;;     "r" #'elfeed-search-fetch
;;     "S" #'elfeed-unjam))

;; (with-eval-after-load "elfeed-show"
;;   (evil-define-key*
;;     'motion elfeed-show-mode-map
;;     "gb" #'elfeed-show-visit
;;     "gj" #'elfeed-show-next
;;     "gk" #'elfeed-show-prev))

(use-package elfeed-tube
  :ensure t
  :after elfeed
  :demand t
  :config
  (elfeed-tube-setup)

  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))
