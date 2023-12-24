;; (use-package dmenu)

;; (defun efs/set-wallpaper ()
;;   (interactive)
;;   ;; NOTE: You will need to update this to a valid background path!
;;   (start-process-shell-command
;;    "feh" nil  "feh --bg-fill /home/chopin/Pictures/Обои/math/303155.jpg"))

;; (use-package exwm
;;   :straight t
;;   :config
;;   (setq exwm-workspace-number 10)
;;   (setq exwm-workspace-offset 20)

;;   (exwm-workspace-switch-create 1)

;;   (require 'exwm-systemtray)
;;   (exwm-systemtray-enable)

;;   (defun efs/configure-window-by-class ()
;;     (interactive)
;;     (pcase exwm-class-name
;;       ("brave-bin" (exwm-workspace-move-window 2))
;;       ("firefox" (exwm-workspace-move-window 2))
;;       ("konsole" (exwm-workspace-move-window 4))
;;       ("Sol" (exwm-workspace-move-window 3))
;;       ("mpv" (exwm-floating-toggle-floating)
;;        (exwm-layout-toggle-mode-line))))
;;   ;; Configure windows as they're created
;;   (add-hook 'exwm-manage-finish-hook 'efs/configure-window-by-class)

;;   ;; Automatically send the mouse cursor to the selected workspace's display
;;   (setq exwm-workspace-warp-cursor t)

;;   ;; Window focus should follow the mouse pointer
;;   (setq mouse-autoselect-window t
;;         focus-follows-mouse t)

;;   (efs/set-wallpaper)

;;   ;; Global keybindings can be defined with `exwm-input-global-keys'.
;;   ;; Here are a few examples:
;;   (setq exwm-input-global-keys
;;         `(
;;           ;; Bind "s-r" to exit char-mode and fullscreen mode.
;;           ([?\s-s] . exwm-reset)
;;           ;; Bind "s-w" to switch workspace interactively.
;;           ([?\s-w] . exwm-workspace-move-window)
;;           ([?\s-h] . windmove-left)
;;           ([?\s-l] . windmove-right)
;;           ([?\s-k] . windmove-up)
;;           ([?\s-j] . windmove-down)
;;           ([?\s-a] . exwm-workspace-switch-to-buffer)
;;           ([?\s-q] . pkill-xinit)
;;           ([?\s-f] . org-roam-node-find)
;;           ([?\s-p] . suspend-session)
;; 	  ([?\s-d] . dmenu)

;;           ;; application keybinds
;;           ([?\s-t] . vterm)
;;           ([?\s-c] . calculator)

;; 	  ;; Switch workspace
;;           ;; ([?\s-w] . exwm-workspace-switch)
;;           ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

;;           ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
;;           ,@(mapcar (lambda (i)
;;                       `(,(kbd (format "s-%d" i)) .
;;                         (lambda ()
;;                           (interactive)
;;                           (exwm-workspace-switch-create ,i))))
;;                     (number-sequence 0 9))))
;;   (setq exwm-randr-workspace-monitor-plist '(1 "HDMI-1" 9 "eDP-1"))


;;   ;; Bind "s-r" to launch applications ('M-&' also works if the output
;;   ;; buffer does not bother you).
;;   ;; ([?\s-r] . app-launcher-run-app)
;;   ;; ([?\s-b] . emms-browser)
;;   ;; Bind "s-<f2>" to "i3lock", a simple X display locker.
;;   ;; ([s-f2] . (lambda ()
;;   ;; 	      (interactive)
;;   ;; 	      (start-process "" nil "i3lock")))

;;   ;; To add a key binding only available in line-mode, simply define it in
;;   ;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
;;   (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;;   (setq exwm-input-simulation-keys
;; 	'(
;;           ;; movement
;;           ([?\C-b] . [left])
;;           ([?\M-b] . [C-left])
;;           ([?\C-f] . [right])
;;           ([?\M-f] . [C-right])
;;           ([?\C-p] . [up])
;;           ([?\C-n] . [down])
;;           ([?\C-a] . [home])
;;           ([?\C-e] . [end])
;;           ([?\M-v] . [prior])
;;           ([?\C-v] . [next])
;;           ([?\C-d] . [delete])
;;           ([?\C-k] . [S-end delete])
;;           ;; cut/paste.
;;           ([?\C-w] . [?\C-x])
;;           ([?\M-w] . [?\C-c])
;;           ([?\C-y] . [?\C-v])
;;           ;; search
;;           ([?\C-s] . [?\C-f])))

;;   (defun pkill-xinit ()
;;     "Kill the xinit process."
;;     (interactive)
;;     (if (y-or-n-p "Are you sure you want to kill the X sesession? ")
;; 	(progn
;;           (shell-command "lxqt-leave")
;;           (message "xinit killed."))
;;       (message "xinit not killed.")))

;;   (defun suspend-session ()
;;     "Suspend the system."
;;     (interactive)
;;     (if (y-or-n-p "Are you sure you want to suspend? ")
;; 	(progn
;;           (shell-command "loginctl suspend")
;;           (message "system suspended."))
;;       (message "system not suspended.")))

;;   ;; Automatically move EXWM buffer to current workspace when selected
;;   (setq exwm-layout-show-all-buffers t)

;;   ;; Display all EXWM buffers in every workspace buffer list
;;   (setq exwm-workspace-show-all-buffers t)

;;   ;; Do not forget to enable EXWM. It will start by itself when things are
;;   ;; ready.  You can put it _anywhere_ in your configuration.
;;   (setenv "GPG_AGENT_INFO" nil)  ;; use emacs pinentry
;;   (setq auth-source-debug t)

;;   (defvar efs/polybar-process nil
;;     "Holds the process of the running Polybar instance, if any")

;;   (defun efs/kill-panel ()
;;     (interactive)
;;     (when efs/polybar-process
;;       (ignore-errors
;; 	(kill-process efs/polybar-process)))
;;     (setq efs/polybar-process nil))

;;   (defun efs/start-panel ()
;;     (interactive)
;;     (efs/kill-panel)
;;     (setq efs/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

;;   (defun efs/send-polybar-hook (module-name hook-index)
;;     (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

;;   (defun efs/send-polybar-exwm-workspace ()
;;     (efs/send-polybar-hook "exwm-workspace" 1))

;;   ;; Update panel indicator when workspace changes
;;   (add-hook 'exwm-workspace-switch-hook 'efs/send-polybar-exwm-workspace)

;;   (use-package desktop-environment
;;     :after exwm
;;     :config (desktop-environment-mode)
;;     :custom
;;     (desktop-environment-brightness-small-increment "5%+")
;;     (desktop-environment-brightness-small-decrement "5%-")
;;     (desktop-environment-brightness-normal-increment "5%+")
;;     (desktop-environment-brightness-normal-decrement "5%-")
;;     (desktop-environment-mode))

;;   (exwm-enable))

;; (setq org-link-frame-setup '((file . find-file)))

;; (provide 'exwm-my-own-settings)
