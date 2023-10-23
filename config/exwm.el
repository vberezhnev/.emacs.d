(use-package dmenu)

(use-package exwm
  :straight t
  :config
  (setq exwm-workspace-number 4)
  (setq exwm-workspace-offset 20)
  ;; (setq exwm-randr-workspace-monitor-plist '(1 "HDMI-1" 9 "eDP-1"))
  
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  ;; All buffers created in EXWM mode are named "*EXWM*". You may want to
  ;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
  ;; are run when a new X window class name or title is available.  Here's
  ;; some advice on this topic:
  ;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
  ;; + For applications with multiple windows (e.g. GIMP), the class names of
					;    all windows are probably the same.  Using window titles for them makes
  ;;   more sense.
  ;; In the following example, we use class names for all windows except for
  ;; Java applications and GIMP.
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                        (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-title))))

  (defun efs/configure-window-by-class ()
    (interactive)
    (pcase exwm-class-name
      ("Brave-browser" (exwm-workspace-move-window 2))
      ("Sol" (exwm-workspace-move-window 3))
      ("mpv" (exwm-floating-toggle-floating)
       (exwm-layout-toggle-mode-line))))
  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook 'efs/configure-window-by-class)

  ;; Automatically send the mouse cursor to the selected workspace's display
  (setq exwm-workspace-warp-cursor t)

  ;; Window focus should follow the mouse pointer
  (setq mouse-autoselect-window t
        focus-follows-mouse t)

  ;; Global keybindings can be defined with `exwm-input-global-keys'.
  ;; Here are a few examples:
  (setq exwm-input-global-keys
        `(
          ;; Bind "s-r" to exit char-mode and fullscreen mode.
          ([?\s-s] . exwm-reset)
          ;; Bind "s-w" to switch workspace interactively.
          ([?\s-w] . exwm-workspace-move-window)
          ([?\s-h] . windmove-left)
          ([?\s-l] . windmove-right)
          ([?\s-k] . windmove-up)
          ([?\s-j] . windmove-down)
          ([?\s-a] . exwm-workspace-switch-to-buffer)
          ([?\s-q] . pkill-xinit)
          ([?\s-f] . org-roam-node-find)
          ([?\s-p] . suspend-session)
	  ([?\s-d] . dmenu)

          ;; application keybinds
          ([?\s-t] . vterm)
          ([?\s-c] . calculator)

          ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
          ;; ,@(mapcar (lambda (i)
          ;;             `(,(kbd (format "s-%d" i)) .
          ;;               (lambda ()
          ;;                 (interactive)
          ;;                 (exwm-workspace-switch-create ,i))))
          ;;           (number-sequence 0 9))
	  ;; Switch workspace
          ;; ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))


  ;; Bind "s-r" to launch applications ('M-&' also works if the output
  ;; buffer does not bother you).
  ;; ([?\s-r] . app-launcher-run-app)
  ;; ([?\s-b] . emms-browser)
  ;; Bind "s-<f2>" to "i3lock", a simple X display locker.
  ;; ([s-f2] . (lambda ()
  ;; 	      (interactive)
  ;; 	      (start-process "" nil "i3lock")))

;; To add a key binding only available in line-mode, simply define it in
;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;; The following example demonstrates how to use simulation keys to mimic
;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
;; and DEST is what EXWM actually sends to application.  Note that both SRC
;; and DEST should be key sequences (vector or string).
(setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))

;; You can hide the minibuffer and echo area when they're not used, by
;; uncommenting the following line.
(setq exwm-workspace-minibuffer-position 'bottom)

(defun pkill-xinit ()
  "Kill the xinit process."
  (interactive)
  (if (y-or-n-p "Are you sure you want to kill the X sesession? ")
      (progn
        (shell-command "lxqt-leave")
        (message "xinit killed."))
    (message "xinit not killed.")))

(defun suspend-session ()
  "Suspend the system."
  (interactive)
  (if (y-or-n-p "Are you sure you want to suspend? ")
      (progn
        (shell-command "loginctl suspend")
        (message "system suspended."))
    (message "system not suspended.")))

;; for lemonbar
;; (defun my/exwm-workspace-list ()
;;   "Return a lemonbar string showing workspace list."
;;   (let* ((num (exwm-workspace--count))
;;          (sequence (number-sequence 0 (1- num)))
;;          (curr (exwm-workspace--position exwm-workspace--current)))
;;     (mapconcat (lambda (i)
;; 		   (format (if (= i curr) "[%%{F#c6a0f6}%d%%{F-}] " "%d ") i))
;; 		 sequence "")
;;     ))

;; (defun my/exwm-report-workspaces-to-lemonbar ()
;;   (with-temp-file "/tmp/panel-fifo"
;;     (insert (format "WIN%s\n" (my/exwm-workspace-list)))))

;; (add-hook 'exwm-workspace-switch-hook 'my/exwm-report-workspaces-to-lemonbar)
;; (add-hook 'exwm-init-hook 'my/exwm-report-workspaces-to-lemonbar)

;; Do not forget to enable EXWM. It will start by itself when things are
;; ready.  You can put it _anywhere_ in your configuration.
(setenv "GPG_AGENT_INFO" nil)  ;; use emacs pinentry
(setq auth-source-debug t)

;; (setq epg-gpg-program "gpg2")  ;; not necessary
;; (require 'epa-file)
;; (epa-file-enable)
;; (setq epa-pinentry-mode 'loopback)
;; (setq epg-pinentry-mode 'loopback)
;; (pinentry-start)

;; (require 'org-crypt)
;; (org-crypt-use-before-save-magic)

(exwm-enable))

(defun bp/send-polybar-hook (module-name hook-index)
  "Generic IPC hook function for communicating with Polybar"
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun bp/send-polybar-exwm-ws-indicator ()
  "Wraps the hook for the 'exwm-ws-indicator' Polybar module"
  (bp/send-polybar-hook "exwm-ws-indicator" 1))

(defun bp/polybar-exwm-ws-indicator ()
  "Switch case to select the appropriate indicator"
  (pcase exwm-workspace-current-index
     (0 "                   ")
     (1 "                   ")
     (2 "                   ")
     (3 "                   ")
     (4 "                   ")
     (5 "                   ")
     (6 "                   ")
     (7 "                   ")
     (8 "                   ")
     (9 "                   ")))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook 'bp/send-polybar-exwm-ws-indicator)

;; (use-package vterm
;;   :straight t)


(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "5%+")
  (desktop-environment-brightness-small-decrement "5%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

;; (use-package browse-kill-ring
;;   :straight t)

;; (use-package app-launcher
;;   :straight '(app-launcher :host github :repo "SebastienWae/app-launcher"))

(provide 'exwm-my-own-settings)
