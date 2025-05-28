(use-package nano-theme
	:ensure t)

(use-package elfeed
  :ensure t
  ;; :commands (elfeed)
  :bind ("C-c x" . elfeed) ; Global keybinding to launch elfeed
  :config
  (setq-default elfeed-search-filter "")
	(setq elfeed-search-trailing-width 15)
	(defun elfeed-search-format-date (date) "")
	
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
