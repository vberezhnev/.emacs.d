(defvar ic/elfeed-external-mode-map (make-sparse-keymap))
;; (define-minor-mode ic/elfeed-external-mode "A minor mode to add external modes `showing` elfeed entry content" (use-local-map ic/elfeed-external-mode-map))

(use-package nano-theme
  :straight t)

(use-package elfeed-summary
  :straight t)

;; (setq my/elfeed-update-timer
;;       (run-at-time nil (* 1 60 60 24) #'elfeed-update))

(use-package elfeed
  :straight t
  ;; :commands (rss)
  ;; :bind ("C-c x" . elfeed) ; Global keybinding to launch elfeed
  ;; :bind (:map elfeed-search-mode-map
  ;;             ("j" . next-line)
  ;;             ("k" . previous-line)
  ;;             ("e" . ic/elfeed-open-in-eww)
  ;; 	      :map elfeed-show-mode-map
  ;;             ("e" . ic/elfeed-show-in-eww))
  
  :bind (:map elfeed-show-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save)
              :map elfeed-search-mode-map
	      ("S" . elfeed-search-live-filter)
              ;; ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save))
  :hook
  (elfeed-show-mode . my/reading-mode-hook)
  :config
  ;; (add-hook 'elfeed-search-mode-hook 'my/elfeed-update-timer)

  (evil-define-key 'normal elfeed-show-mode-map (kbd "j") 'my-view-down)
  (evil-define-key 'normal elfeed-show-mode-map (kbd "k") 'my-view-up)
  (evil-define-key 'evil elfeed-search-mode-map (kbd "o") 'ic/elfeed-open-in-eww)

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
      "S" #'elfeed-search-live-filter
      "F" #'elfeed-tube-fetch
      [remap save-buffer] #'elfeed-tube-save))
  ;; Keybindings for elfeed-show-mode
  ;; (with-eval-after-load "elfeed-show"
  ;;   (evil-define-key* 'motion elfeed-show-mode-map
  ;;     "gb" #'elfeed-show-visit
  ;;     "gj" #'elfeed-show-next
  ;;     "gk" #'elfeed-show-prev))
  )

;; (add-hook 'emacs-startup-hook (run-at-time nil (* 1 60 60 24) #'elfeed-update)) ;; (lambda () (run-at-time 5 5 'elfeed-update))

;; (run-at-time (* 60 25) 'elfeed-update)


(use-package elfeed-org
  :straight t
  :demand t
  :init
  (elfeed-org)
  :config
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

(use-package stripes
  :straight (:type built-in)
  :load-path "~/.emacs.d/lisp/packages/nano-elfeed/")

(use-package relative-date
  :straight (:type built-in)
  :load-path "~/.emacs.d/lisp/packages/nano-elfeed/")

(use-package nano-elfeed
  :straight (:type built-in)
  :load-path "~/.emacs.d/lisp/packages/nano-elfeed/")

(use-package elfeed-tube
  :straight t
  :after elfeed
  :demand t
  :config
  ;; (setq elfeed-tube-invidious-url "https://invidious.nerdvpn.de/")
  (elfeed-tube-setup))

(defun elfeed-show-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((browse-url-browser-function #'eww-browse-url))
    (elfeed-show-visit use-generic-p)))

(defun elfeed-search-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((browse-url-browser-function #'eww-browse-url))
    (elfeed-search-browse-url use-generic-p)))

(define-key elfeed-show-mode-map (kbd "B") 'efleed-show-eww-open)
(define-key elfeed-search-mode-map (kbd "B") 'efleed-search-eww-open)

;;;###autoload
(defun ic/elfeed-delete-external-windows ()
  (mapcar (lambda (w) (when w (delete-window w))) (mapcar (lambda (b) (get-buffer-window b 'visible)) (ic/elfeed-external-buffer-list))))

;;;###autoload
(defun ic/elfeed-delete-non-search-windows ()
  (interactive)
  "Delete all elfeed non search buffers."
  ;; External
  (condition-case nil
      (ic/elfeed-delete-external-windows)
    (error nil)))

;;;###autoload
(defun ic/mark-current-as-read ()
  (interactive)
  "Mark current entry as read."
  (let ((current (elfeed-search-selected :ignore-region)))
    (elfeed-untag current 'unread)
    (elfeed-search-update-entry current)
    (elfeed-db-save-safe)))

;;;###autoload
(defun ic/elfeed-open-in-eww (entry)
  "Display the currently selected item in eww."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (ic/mark-current-as-read)
  (ic/elfeed-delete-non-search-windows)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (when (derived-mode-p 'elfeed-search-mode)) ;;  (ic/split-and-follow-vertically)
      (eww link)
      (rename-buffer (format "*elfeed eww %s*" link))
      (ic/elfeed-external-mode))))

;;;###autoload
(defun ic/elfeed-show-in-eww ()
  "Display the currently shown item in xwidget-webkit-browser."
  (interactive)
  (require 'elfeed-show)
  (when (elfeed-entry-p elfeed-show-entry)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (eww link)
      (rename-buffer (format "*elfeed eww %s*" link))
      (ic/elfeed-external-mode))))

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

;;; autoload
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

;;; autoload
(defun my/toggle-cursor ()
  (interactive)
  (if (null cursor-type)
      (my/show-cursor-evil)
    (my/hide-cursor-evil)))

;; (defun my/elfeed-update-timer)

(defun my/reading-mode-hook ()
  (my/centre-visual-fill-on)
  ;; (my/hide-cursor-evil)
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
