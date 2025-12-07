;;; org-mode.el --- Org-mode configuration -*- lexical-binding: t; -*-

;; Org: Load for org-related commands
(use-package org
  ;; :straight (:type built-in)
  :straight t
  ;; :straight (:host github :repo "bzg/org-mode")
  ;; :commands (org-agenda org-capture org-store-link org-footnote-new org-clock-goto org-clock-out)
  :bind (("C-c u" . calendar)
	 ("C-c C-x C-o" . org-clock-out)
	 ("C-c C-x C-j" . org-clock-goto)
	 :map org-mode-map
	 ("C-c l" . org-store-link)
         ("C-c f" . org-footnote-new))
  :init
  (setq org-ellipsis " ⤵"
	org-log-done 'time
	org-hide-leading-stars t
	org-log-into-drawer t
	org-startup-folded t
	org-pretty-entities t
	org-startup-indented t
	org-adapt-indentation t
	org-hide-macro-markers t
	org-hide-block-startup nil
	org-cycle-separator-lines 2
	org-startup-with-inline-images t
	org-display-remote-inline-images t
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
	org-log-redeadline t
	org-log-reschedule t
	org-tag-alist
	'((:startgroup)
          ("@coding"     . ?c)
          ("@reading"    . ?r)
          ("@video"      . ?v)
          ("@writing"    . ?w)
          ("@thinking"   . ?t)
          ("@planning"   . ?p)
          ("@research"   . ?s)
          (:endgroup)

          ("@interviewing" . ?i)
          ("@zettelkasten" . ?z)

          ;; Темы
          ("rust"        . ?R)
          ("c++"         . ?C)
          ("nestjs"      . ?N)
          ("solana"      . ?S)
          ("db"          . ?D)
          ("philosophy"  . ?P)
          ("math"        . ?M)
          ("psychology"  . ?Y)
          ("productivity". ?U)
          ("chinese"     . ?H))
	)
  :config
  (with-eval-after-load 'org
    (setq org-confirm-babel-evaluate nil)
    (require 'ob-tangle)
    (require 'org-tempo)
    (add-hook 'org-babel-after-execute-hook (lambda ()
					      (when org-inline-image-overlays
						(org-redisplay-inline-images))))
    (add-to-list 'org-modules 'org-tempo t))

  (defun my-org-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                             :height 1.5))
  (add-hook 'org-mode-hook 'my-org-font-setup)

  ;; (set-face-attribute 'org-block nil            :foreground nil :inherit
  ;; 		      'fixed-pitch :height 0.85)
  ;; (set-face-attribute 'org-code nil             :inherit '(shadow fixed-pitch) :height 0.85)
  ;; (set-face-attribute 'org-indent nil           :inherit '(org-hide fixed-pitch) :height 0.85)
  ;; (set-face-attribute 'org-verbatim nil         :inherit '(shadow fixed-pitch) :height 0.85)
  ;; (set-face-attribute 'org-special-keyword nil  :inherit '(font-lock-comment-face
  ;; 							   fixed-pitch))
  ;; (set-face-attribute 'org-meta-line nil        :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-checkbox nil         :inherit 'fixed-pitch)

  (require 'org-duration)
  (defun my/org-update-sleep-duration ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*\\*\\* " nil t)
	(let ((start (org-entry-get nil "SLEEP_START"))
	      (end (org-entry-get nil "SLEEP_END")))
	  (when (and start end)
	    (let* ((minutes (round (/ (float-time (time-subtract
						   (org-time-string-to-time end)
						   (org-time-string-to-time start)))
				      60)))
		   (dur (org-duration-from-minutes minutes)))
	      (org-entry-put nil "DURATION" (format "%-14s%s" "" dur))))))))


  (setq org-capture-templates
	'(("s" "Sleep Entry" entry (file+headline "~/Org/sleep.org" "Sleep")
	   "* %<%Y-%m-%d %a> :W%:
:PROPERTIES:
:SLEEP_START: %^t
:SLEEP_END:   %^t
:QUALITY:     %^{Quality (1-10)|8|9|7|6|5|4|3|2|1}
:END:
%^?" :prepend t))))

;; Custom functions (unchanged)
(defun org-dblock-write:time-requirements (params)
  "Generate a table showing daily time requirements and progress for categories."
  (let* ((day-of-week (upcase (format-time-string "%^a")))
         (required-property (concat "REQUIRED_TIME_" day-of-week))
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
           (habits-clean (replace-regexp-in-string "^─+\n" "" habits-report))
           (day-clean (replace-regexp-in-string "^─+\n" "" day-results)))
      (setq org-habit-graph-column original-habit-column)
      (insert "** Habits report" habits-clean "\n\n** Day results" day-clean))))

;; Org-babel languages
(use-package ob-typescript
  :straight t
  :after org
  :defer t)

(use-package ob-rust
  :straight t
  :after org
  :defer t)

(use-package ob-solidity
  :straight t
  :after org
  :defer t)

(use-package ob-sql-mode
  :straight t
  :after org
  :defer t)

(use-package ob-restclient
  :straight t
  :after org
  :defer t)

(setq org-ditaa-jar-path "/home/berezhnev/.emacs.d/lisp/packages/ditaa/ditaa.jar")

;; Load org-babel languages (unchanged)
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (js         . t)
     (ditaa      . t)
     (shell      . t)
     (python     . t)
     (rust       . t)
     (C          . t)
     (sql        . t)
     (latex      . t)
     (restclient . t))))

;; Org-modern: Load for org-mode
(use-package org-modern
  :straight t
  :after org
  :commands (org-modern-mode global-org-modern-mode)
  :hook (org-mode . org-modern-mode)
  :config
  (global-org-modern-mode 1)
  (setq org-catch-invisible-edits 'show-and-error
        org-modern-radio-target '("❰" t "❱")
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
        org-modern-priority-faces (quote ((?A :background "red" :foreground "black")
                                          (?B :background "dark orange" :foreground "black")
                                          (?C :background "tan" :foreground "black")))
        org-modern-tag t
        org-modern-timestamp nil
        org-modern-statistics t
        org-modern-table t
        org-modern-tag-faces (quote (("@zettelkasten" :background "#DC1FFF" :foreground "#000000")
                                     ("@coding" :background "#00bfff" :foreground "#000000")
                                     ("@business" :background "#00bfff" :foreground "#000000")
                                     ("@crypto" :background "#FFD700" :foreground "#000000")
                                     ("@health" :background "#0b8043" :foreground "#ffffff")
                                     ("@interview" :background "#CE412B" :foreground "#ffffff")

                                     ("rust" :background "#CE412B" :foreground "#ffffff")
                                     ("go" :background "#CE412B" :foreground "#ffffff")
                                     ("devops" :background "#CE412B" :foreground "#ffffff")

				     ))
        org-modern-horizontal-rule "──────────────────────────────────────────────────────────────────────────────────────────"
        org-modern-hide-stars " "
        org-modern-keyword "‣"))

;; Sound-wav: Load for org-pomodoro
(use-package sound-wav
  :straight t
  :commands (sound-wav-play)
  :defer t)

;; Powershell: Load for org-pomodoro
(use-package powershell
  :straight t
  :commands (powershell)
  :defer t)

;; Org-pomodoro: Load for pomodoro commands
(use-package org-pomodoro
  :straight t
  :after org
  :commands (org-pomodoro my/org-pomodoro)
  :bind (("C-c k" . my/org-pomodoro))
  :config
  (setq org-pomodoro-audio-player "aplay")
  :init
  (setq ;; org-pomodoro-audio-player "mpv" ;;(or (executable-find "mpv") (executable-find "aplay"))
   org-pomodoro-play-sounds t
   org-pomodoro-keep-killed-pomodoro-time t
   org-pomodoro-format "👨‍💻 %s"
   org-pomodoro-short-break-format "🍵 Short Break %s"
   org-pomodoro-long-break-format "✋ Long Break %s"
   org-pomodoro-start-sound-p t
   org-pomodoro-start-sound "/home/berezhnev/.emacs.d/sounds/bell.wav"
   org-pomodoro-finished-sound-p t
   org-pomodoro-finished-sound "/home/berezhnev/.emacs.d/sounds/bell.wav"
   org-pomodoro-manual-break t
   org-pomodoro-overtime-sound-p t
   org-pomodoro-overtime-sound "/home/berezhnev/.emacs.d/sounds/bell.wav"
   org-pomodoro-length 40
   org-pomodoro-short-break-length 5
   org-pomodoro-long-break-length 15
   org-pomodoro-long-break-frequency 2))

;; Custom pomodoro function (unchanged)
(defun my/org-pomodoro ()
  (interactive)
  (org-pomodoro '(4)))

;; Org-timed-alerts: Load for org-mode
(use-package org-timed-alerts
  :straight (:host github :repo "legalnonsense/org-timed-alerts" :branch "master" :files ("*.el" "out"))
  :after org
  :commands (org-timed-alerts-mode)
  :hook (org-mode . org-timed-alerts-mode)
  :custom
  (org-timed-alerts-alert-function #'alert)
  (org-timed-alerts-tag-exclusions nil)
  (org-timed-alerts-default-alert-props nil)
  (org-timed-alerts-warning-times '(-30 -15 -5))
  (org-timed-alerts-agenda-hook-p t)
  (org-timed-alert-final-alert-string "IT IS %alert-time\n\n%todo %headline")
  (org-timed-alert-warning-string (concat "%todo %headline\n at %alert-time")))

(use-package org-alert
  :straight t
  :config
  (setq alert-default-style 'libnotify
	org-alert-notification-title "ORG REMINDER"
	org-alert-headline "Task Reminder"
	org-alert-time '(deadline scheduled)
	org-alert-active-margin 0 ;; Напоминать о всех задачах, которые на сегодня
	org-alert-interval 300) ;; каждые 5 минут проверка (можно 60–120)
  (org-alert-enable))

;; Emacsql: Load for org dependencies
(use-package emacsql
  :straight t
  :after org
  :defer t)

;; Org-download: Load for image handling
(use-package org-download
  :straight t
  :after org
  :commands (org-download-clipboard org-download-image)
  :bind (:map org-mode-map
	      ("C-x p m" . org-download-clipboard)
	      ("C-x p o" . org-download-image))
  :config
  (setq-default org-download-image-dir "./assets-org/"))

;; Org-cliplink: Load for link handling
(use-package org-cliplink
  :straight t
  :after org
  :commands (org-cliplink)
  :bind (("C-x p i" . org-cliplink))
  :config
  (setq org-cliplink-max-length 800))

(setq org-gtd-update-ack "3.0.0")

;; Org-gtd: Load for GTD workflows
(use-package org-gtd
  :straight (:type git :host github :repo "Trevoke/org-gtd.el")
  :demand t
  ;; :after org
  ;; :commands (org-gtd-capture org-gtd-engage org-gtd-engage-grouped-by-context org-gtd-process-inbox org-gtd-organize)
  :bind (("C-c d c" . org-gtd-capture)
         ("C-c d e" . org-gtd-engage)
         ("C-c d r" . org-gtd-engage-grouped-by-context)
         ("C-c d p" . org-gtd-process-inbox)
         :map org-gtd-clarify-map
         ("C-c c" . org-gtd-organize))
  :custom
  (org-gtd-directory "~/Org/agenda/GTD/")
  (org-edna-use-inheritance t)
  (org-gtd-update-ack "3.0.0")
  (org-gtd-areas-of-focus '("PERSONAL" "CORE" "ASCENT" "CHINESE"))
  (org-gtd-organize-hooks '(org-gtd-set-area-of-focus org-set-tags-command))
  (org-gtd-clarify-show-horizons t)
  (org-gtd-horizons-file "horizons.org")
  :config
  ;; (org-gtd-oops)
  (org-edna-mode))

;; Org-clock-budget: Load for budgeting
(use-package org-clock-budget
  :ensure nil
  ;; :straight (:host github :repo "vberezhnev/org-clock-budget" :branch "master")
  :straight (:host github :repo "Fuco1/org-clock-budget" :branch "master")
  :after org
  :commands (org-clock-budget-report)
  :config
  (setq org-clock-budget-daily-budgetable-hours 12)
  (setq org-clock-budget-intervals '(("BUDGET_WEEK" org-clock-budget-interval-this-week))))

;; Org-appear: Load for org-mode
(use-package org-appear
  :straight t
  :after org
  :commands (org-appear-mode)
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t
        org-appear-autolinks 'just-brackets))

;; (use-package org-tempo
;;   :after org
;;   :config
;;   (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
;;   (add-to-list 'org-structure-template-alist '("r" . "src rust"))
;;   (add-to-list 'org-structure-template-alist '("j" . "src java"))
;;   (add-to-list 'org-structure-template-alist '("k" . "src kotlin"))
;;   (add-to-list 'org-structure-template-alist '("sh" . "src sh")))

(require 'epa)
(when (eq system-type 'darwin)
  (setf epa-pinentry-mode 'loopback))
(setq auth-sources '("~/.authinfo"))
;; (setq auth-source-debug t)

(use-package org-caldav
  :straight t
  :config
  (setq org-caldav-url "http://31.200.229.59:5232/berezhnev")
  (setq org-caldav-calendar-id "ec7af6a8-d5f3-060d-2031-615829778e8f")
  (setq org-caldav-inbox "~/Org/agenda/timeblock.org")
  (setq org-caldav-files '("~/Org/agenda/timeblock.org"
			   "~/Org/agenda/GTD/org-gtd-tasks.org"))
  (setq org-caldav-uuid-extension ".ics")
  (setq org-icalendar-timezone "Asia/Vladivostok") ; Adjust to your timezone
  (setq org-caldav-sync-direction 'twoway) ; Sync both ways (Org <-> CalDAV)
  (setq org-caldav-exclude-tags '("habit"))
  (setq org-caldav-delete-org-entries 'always) ; Remove Org entries if deleted on server
  (setq org-caldav-delete-calendar-entries 'always) ; Remove CalDAV entries if deleted in Org
  (setq org-caldav-save-directory "~/Org/Caldav/")) ; Directory for sync state

;; (use-package org-popup-posframe
;;   :straight (:host github
;;              :repo "A7R7/org-popup-posframe"
;;              :branch "main")
;;   :init
;;   (org-popup-posframe-mode 1))

;; (load-file "~/.emacs.d/lisp/packages/org-popup-posframe/org-popup-posframe.el")
;; (org-popup-posframe-mode 1)

;; (use-package repeat-todo
;;   :straight (:host github
;; 		   :repo "cashpw/repeat-todo"
;; 		   :branch "main")
;;   :after org)

(use-package org-habit-plus
  :straight (:host github
		   :repo "myshevchuk/org-habit-plus"
		   :branch "master")
  :after org)

(use-package org-timeblock
  :straight t
  :bind
  (:map global-map
 	("C-c s" . org-timeblock))
  :config
  (setq org-timeblock-files '("~/Org/agenda/timeblock.org"
			      "~/Org/agenda/GTD/org-gtd-tasks.org")
	org-timeblock-inbox-file "~/Org/agenda/timeblock.org"
	org-timeblock-span 4
	org-timeblock-scale-options '(6 . 25)
	org-timeblock-show-future-repeats t))

;; (use-package timeblock
;;   :straight (:host github
;; 		   :repo "ichernyshovvv/timeblock.el"
;; 		   :branch "master"))

(use-package org-contacts
  :straight t
  :custom
  (org-contacts-files '("~/Org/Org-roam/")))

;; (use-package org-upcoming-modeline
;;   :ensure t
;;   :init
;;   (org-upcoming-modeline-mode)
;;   :config
;;   (setq org-upcoming-modeline-format (lambda (ms mh) (format "📅 %s - %s" ms mh))))




;; нахуй этот ваш org-clock-in
;; (with-eval-after-load 'org
;;   (define-key org-mode-map (kbd "C-c C-x C-i") nil)
;;   (define-key org-mode-map (kbd "C-c C-x C-o") nil))
;; (setq org-clock-auto-clockout-timer nil)
;; (setq org-clock-mode-line-total 'none)
;; (setq org-clock-persist nil)
;; (setq org-clock-into-drawer nil)

(use-package dash
  :straight t)

(use-package org-now
  :straight (:host github :repo "alphapapa/org-now" :branch "master" :files ("*.el" "out"))
  :after org  ;; Убедитесь, что org-mode загружен перед org-now
  :config
  (setq org-now-location '("~/Org/agenda/GTD/org-gtd-tasks.org" "TASKS IN PROGRESS"))
  (setq org-now-window-side 'right)  ;; Показывать панель справа
  ;; (setq org-now-default-cycle-level 2)  ;; Уровень раскрытия заголовков
  ;; (setq org-now-no-other-window t)  ;; Не переключаться на панель через `other-window`
  ;; Привязка клавиш
  :general
  (:keymaps 'org-mode-map
            :prefix "M-SPC"
            "rl" #'org-now-link
            "rn" #'org-now-refile-to-now
            "rp" #'org-now-refile-to-previous-location))

;;;;;;;;

(defun my-gtd-stale-next-to-someday (&optional days)
  "Move stale NEXT tasks to SOMEDAY/Incubated if older than DAYS.
Default is 14 days."
  (interactive "P")
  (let ((days (or (and days (prefix-numeric-value days)) 14))
        (cutoff (float-time (time-subtract (current-time)
                                           (days-to-time days))))
        ;; путь к SOMEDAY/Incubated
        (incubated-file (expand-file-name "incubated.org" org-gtd-directory)))
    (org-map-entries
     (lambda ()
       (let* ((ts (org-entry-get (point) "ORG_GTD_TIMESTAMP"))
              (time (when ts (float-time (org-time-string-to-time ts)))))
         (when (and time
                    (< time cutoff)) ; протухло
           (message "Incubating stale NEXT: %s" (org-get-heading t t))
           ;; ставим TODO → TODO (обычный)
           (org-todo org-gtd-todo)
           ;; рефайлим в Incubated
           (org-refile nil nil
                       (list (org-get-heading t t)
                             incubated-file
                             nil
                             nil)))))
     (format "TODO=\"%s\"" org-gtd-next)
     `(org-gtd-agenda-files))))


(use-package request
  :straight t)

(use-package org-social
  :straight (:host github :repo "tanrax/org-social.el" :branch "main")
  :demand t
  :config
  (org-social-add-account "vberezhnev"
                          :file (getenv "ORG_SOCIAL_FILE_VBEREZHNEV")
                          :relay "https://relay.org-social.org/"
                          :public-url "http://host.org-social.org/vberezhnev/social.org")

  (org-social-add-account "obersturmbannburger"
                          :file (getenv "ORG_SOCIAL_FILE_OBERSTURMBANNBURGER")
                          :relay "https://relay.org-social.org/"
                          :public-url "http://host.org-social.org/obersturmbannburger/social.org")

  (setq org-social-relay "https://relay.org-social.org/")
  (setq org-social-my-public-url "http://host.org-social.org/obersturmbannburger/social.org")

  (load-file "~/.emacs.d/lisp/packages/org-social.el/org-social-ui.el")
  (load-file "~/.emacs.d/lisp/packages/org-social.el/ui/org-social-ui-core.el")
  (load-file "~/.emacs.d/lisp/packages/org-social.el/ui/org-social-ui-utils.el")
  (load-file "~/.emacs.d/lisp/packages/org-social.el/ui/org-social-ui-components.el")
  )

(provide 'org-mode)
;;; org-mode.el ends here
