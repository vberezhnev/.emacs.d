(use-package org
  :straight (:type built-in)
  :bind (("C-c C-x C-j" . org-clock-goto))
  :ensure nil
  :bind (("C-c l"               . org-store-link)
         ("C-c f"               . org-footnote-new)
	 ("C-c u"               . calendar))
  :config
  (setq
   org-ellipsis " ⤵" ;; ⤵, ᗐ, ↴, ▼, ▶, ⤵, ▾
   ;; org-roam-v2-ack t                 ; anonying startup message
   org-log-done 'time                ; I need to know when a task is done
   org-hide-leading-stars t
   org-log-into-drawer t
   org-startup-folded t
   org-pretty-entities t
   org-startup-indented t
   org-adapt-indentation t
   org-hide-macro-markers t
   org-hide-block-startup nil
   ;; org-src-fontify-natively t
   ;; org-src-tab-acts-natively t
   org-cycle-separator-lines 2
   org-startup-with-inline-images t
   org-display-remote-inline-images t
   ;; org-src-preserve-indentation nil
   ;; org-edit-src-content-indentation 2
   org-fontify-quote-and-verse-blocks t
   org-export-with-smart-quotes t

   org-checkbox-hierarchical-statistics nil
   org-read-date-prefer-future 'time
   org-agenda-todo-ignore-scheduled 'future
   org-agenda-tags-todo-honor-ignore-options t
   org-agenda-todo-ignore-with-date t
   org-image-actual-width '(300)
   org-log-done (quote time)
   org-duration-format 'h:mm
   ;; Don't log the time a task was rescheduled or redeadlined.
   org-log-redeadline t ; changed
   org-log-reschedule t
   org-tag-alist '(("@chinese" . ?c)
		   ("@core" . ?r)
		   ("@ascent" . ?a)
		   ("@idea" . ?i)
		   ("@rust" . ?t)
		   ("@solana" . ?s)))

  (with-eval-after-load 'org
    (setq org-confirm-babel-evaluate nil)
    (require 'org-tempo)

    (add-hook 'org-babel-after-execute-hook (lambda ()
					      (when org-inline-image-overlays
						(org-redisplay-inline-images))))
    (add-to-list 'org-modules 'org-tempo t)))

(defun org-dblock-write:time-requirements (params)
  "Generate a table showing daily time requirements and progress for categories."
  (let* ((day-of-week (upcase (format-time-string "%^a")))
         (required-property (concat "REQUIRED_TIME_" day-of-week))
         ;; Новые категории
         (categories '("CHINESE" "CORE" "ASCENT"))
         (today-start (format-time-string "%Y-%m-%d"))
         (today-end (format-time-string "%Y-%m-%d" (time-add (current-time) 86400))))

    (insert "| Category   | Required | Actual  | Progress  |\n")
    (insert "|------------+----------+---------+-----------|\n")

    (dolist (category categories)
      (let ((required 0.0)
            (actual 0.0))
        (org-map-entries
         (lambda ()
           (let* ((cat (org-entry-get (point) "CATEGORY"))
                  (req (org-entry-get (point) required-property)))
             (when (and req (string= cat category))
               (setq required (string-to-number req)))))
         nil 'file)
        (setq actual (/ (float (org-clock-sum today-start today-end
                                              (lambda ()
                                                (string= (org-entry-get nil "CATEGORY")
                                                         category))))
                        60.0))
        (let ((progress (if (> required 0.0)
                            (* 100.0 (/ actual required))
                          0.0)))
          (insert (format "| %-10s | %8.1f | %7.1f | %8.1f%% |\n"
                          category required actual progress)))))

    (insert "|------------+----------+---------+-----------|")))


(global-set-key (kbd "C-c C-x o") 'org-clock-out)
(global-set-key (kbd "C-c C-x j") 'org-clock-go-to)

;; (add-to-list 'load-path "~/Templates2/Lisp/org-habit-enhanced")
;; (require 'org-habit-core)
;; (require 'org-habit-enhanced)
;; (require 'org-habit-stats)
;; (require 'org-habit-quest)
;; (require 'org-habit-tasks)
;; (require 'org-habit-market)
;; (hq-setup)

(defun my/insert-daily-reports ()
  "Вставить отчеты habits и day results в текущий буфер с измененными параметрами отображения."
  (interactive)
  (let ((original-habit-column org-habit-graph-column))
    (setq org-habit-graph-column 38)
    (let* ((habits-report (save-window-excursion
			    (with-temp-buffer
			      (org-agenda nil "x")
			      (buffer-string))))
           (day-results (save-window-excursion
			  (with-temp-buffer
			    (org-agenda nil "d")
			    (buffer-string))))
           ;; Удаляем разделители из обоих отчетов
           (habits-clean (replace-regexp-in-string "^─+\n" "" habits-report))
           (day-clean (replace-regexp-in-string "^─+\n" "" day-results)))
      (setq org-habit-graph-column original-habit-column)
      (insert "** Habits report" habits-clean "\n\n** Day results" day-clean))))
;; (use-package tsc
;;   :ensure t)

(use-package ob-typescript
  :ensure t)
(use-package ob-rust
  :ensure t)
(use-package ob-solidity
  :ensure t)
(use-package ob-sql-mode
  :ensure t)
(use-package ob-restclient
  :ensure t)
(use-package gnuplot
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (js         . t)
   ;; (solidity   . t)
   ;; (typescript . t)
   (shell      . t)
   (python     . t)
   (rust       . t)
   (C          . t)
   (sql        . t)
   (latex      . t)
   (restclient . t)
   (gnuplot    . t)))

(setq )

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :after org
  :ensure t
  :init
  (global-org-modern-mode 1)
  :config
  (setq org-catch-invisible-edits 'show-and-error
	;; org-special-ctrl-a/e t
	;; Appearance
	org-modern-radio-target    '("❰" t "❱")
	org-modern-internal-target '("↪ " t "")
	org-modern-block-name '((t . t)
  				("src" "ϰ" "ϰ")
				("quote" "❝" "❞"))
	org-modern-progress t
	org-modern-statistics nil
	org-modern-todo t
	org-modern-todo-faces (quote (("TODO" :background "indian red" :foreground "white" :weight bold)
  				      ("NEXT" :background "sky blue" :foreground "black" :weight bold)
  				      ("WAIT" :background "olive drab" :foreground "black" :weight bold)
  				      ("DONE" :background "pale green" :foreground "black" :weight bold)
  				      ("CNCL" :background "dark red" :foreground "white" :weight bold)))
	org-modern-priority t
	org-modern-priority-faces (quote ((?A :background "red"
  					      :foreground "black")
  					  (?B :background "dark orange"
  					      :foreground "black")
  					  (?C :background "tan"
  					      :foreground "black")))
	org-modern-tag t
	org-modern-timestamp nil
	org-modern-statistics t
	;; org-modern-table t
	org-modern-tag-faces (quote (("@article" :background "#0b8043" :foreground "#000000")
  				     ("@mathematics" :background "#bc8f8f" :foreground "#000000")
				     ("blockchain" :background "#f5511d" "#000000")
  				     ("solana" :background "#DC1FFF" :foreground "#000000")
  				     ("rust" :background "#CE412B" :foreground "#000000")
  				     ("go" :background "#00bfff" :foreground "#00000")))
	org-modern-horizontal-rule "──────────────────────────────────────────────────────────────────────────────────────────"
	org-modern-hide-stars " "
	org-modern-keyword "‣"
	org-modern-table t))
;; (global-org-modern-mode t)

;;(frostyx/guix :install "alsa-utils")

(use-package sound-wav
  :ensure t
  :demand t) ;; dep for org-pomodoro

(use-package powershell
  :ensure t
  :demand t) ;; dep for org-pomodoro

(use-package org-pomodoro
  :ensure t
  :bind (("C-c k" . my/org-pomodoro))
  :config
  (setq org-pomodoro-audio-player (or (executable-find "aplay") (executable-find "afplay"))
        org-pomodoro-play-sounds t           ; Determines whether soudns are played or not
	org-pomodoro-keep-killed-pomodoro-time t
	org-pomodoro-format " %s"
	org-pomodoro-short-break-format " Short Break %s"
	org-pomodoro-long-break-format  " Long Break %s"
	;; org-pomodoro-finished-sound-p t
        ;; org-pomodoro-start-sound "/home/vberezhnev/.emacs.d/sounds/bell.mp3"

        org-pomodoro-start-sound-p t         ; Determine whether to play a sound when a pomodoro started
        org-pomodoro-start-sound (expand-file-name "sounds/bell.wav" user-emacs-directory)

        org-pomodoro-finished-sound-p t      ; Determines whether to play a sound when a pomodoro finished
        org-pomodoro-finished-sound (expand-file-name "sounds/bell.wav" user-emacs-directory)

        org-pomodoro-manual-break t          ; Whether the user needs to exit manually from a running pomodoro to enter a break
        org-pomodoro-overtime-sound-p t      ; Determines whether to play a sound when a pomodoro starts to run overtime
        org-pomodoro-overtime-sound (expand-file-name "sounds/bell.wav" user-emacs-directory)

	org-pomodoro-length 40
	org-pomodoro-short-break-length 5
	org-pomodoro-long-break-length 15
	org-pomodoro-long-break-frequency 3))

(defun my/org-pomodoro ()
  (interactive)
  (org-pomodoro '(4)))

(use-package org-timed-alerts
  :straight (:host github
		   :repo "legalnonsense/org-timed-alerts"
		   :branch "master" :files ("*.el" "out"))
  :after (org)
  :custom
  (org-timed-alerts-alert-function #'alert)
  (org-timed-alerts-tag-exclusions nil)
  (org-timed-alerts-default-alert-props nil)
  (org-timed-alerts-warning-times '(-30 -15 -5))
  (org-timed-alerts-agenda-hook-p t)
  (org-timed-alert-final-alert-string "IT IS %alert-time\n\n%todo %headline")
  (org-timed-alert-warning-string (concat "%todo %headline\n at %alert-time"))
  :config
  (add-hook 'org-mode-hook #'org-timed-alerts-mode))

(use-package emacsql
  :ensure t)
;; (use-package sqlite3
;;   :ensure t)

(use-package restclient
  :ensure t)

(use-package org-download
  :ensure t
  :bind (:map org-mode-map
              ("C-x p m"    . org-download-clipboard)
              ("C-x p o"    . org-download-image))
  :config
  (setq-default org-download-image-dir "./assets-org/"))

(use-package org-cliplink
  :ensure t
  :config
  (setq org-cliplink-max-length 800)
  (global-set-key (kbd "C-x p i") 'org-cliplink))

(setq org-gtd-update-ack "3.0.0")

(use-package org-gtd
  :ensure t
  :straight (org-gtd :type git
                     :host github
                     :repo "trevoke/org-gtd.el")
  :custom
  (org-gtd-directory "~/Org/agenda/GTD/")
  (org-edna-use-inheritance t)
  (org-gtd-update-ack "3.0.0")
  (org-gtd-areas-of-focus '("PERSONAL" "CORE" "ASCENT" "EGE"))
  (org-gtd-organize-hooks '(org-gtd-set-area-of-focus org-set-tags-command))
  (org-gtd-clarify-show-horizons t)
  (org-gtd-horizons-file "horizons.org")
  :config
  (org-edna-mode)
  :bind (;;("C-c d c" . (lambda () (interactive) (org-gtd-capture nil "i")))
	 ("C-c d c" . org-gtd-capture)
	 ("C-c d e" . org-gtd-engage)
	 ("C-c d r" . org-gtd-engage-grouped-by-context)
	 ("C-c d p" . org-gtd-process-inbox)
	 :map org-gtd-clarify-map
	 ("C-c c" . org-gtd-organize)))

(use-package org-clock-budget
  :quelpa (org-clock-budget :fetcher github :repo "Fuco1/org-clock-budget" :branch "master")
  :ensure t
  :config
  (setq org-clock-budget-daily-budgetable-hours 12)
  (setq org-clock-budget-intervals '(("BUDGET_WEEK" org-clock-budget-interval-this-week))))

(use-package org-appear
  :ensure t
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t
        org-appear-autolinks 'just-brackets))
