;;; popup-term.el --- Popper-powered popup terminal  -*- lexical-binding:t; -*-
;;; Commentary:
;;  C-c t  ->  opens a terminal in a Popper window at the bottom,
;;             selects it, and auto-closes when the shell exits.
;;
;;  Implementation notes
;;  --------------------
;;  • The term buffer is created *without* being displayed, using
;;    `make-term` + `term-mode`, so no window is disturbed.
;;  • Popper then shows the buffer via `pop-to-buffer`, guaranteeing
;;    bottom placement and immediate focus (`popper-select-popup t`).

;;; Code:

;;;; --- dependencies ---------------------------------------------------------
(require 'use-package)                     ;assume bootstrapped elsewhere

;;;; --- popper configuration -------------------------------------------------
(use-package popper
  :ensure t
  :init
  (setq popper-reference-buffers
        '("^\\*\\(ansi-\\)?term.*\\*$" term-mode
          "^\\*vterm.*\\*$"             vterm-mode))
  (setq popper-select-popup   t                          ;focus popup
        popper-display-function #'popper-display-popup-at-bottom)
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :config
  (popper-mode  +1)
  (popper-echo-mode +1))

;;;; --- helpers --------------------------------------------------------------
(defun popup-term--sentinel (proc _event)
  "Auto-kill the terminal PROC’s buffer and its windows on exit."
  (when (memq (process-status proc) '(exit signal closed))
    (let* ((buf  (process-buffer proc))
           (wins (and buf (get-buffer-window-list buf nil t))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (dolist (w wins) (when (window-live-p w) (delete-window w))))))

(defun popup-term--attach-sentinel ()
  "Attach `popup-term--sentinel` to the current term subprocess."
  (when-let ((proc (get-buffer-process (current-buffer))))
    (set-process-sentinel proc #'popup-term--sentinel)))

(add-hook 'term-exec-hook #'popup-term--attach-sentinel)

(defun popup-term--find-shell ()
  "Return an absolute path to a *usable* shell binary, or error."
  (or (and (getenv "SHELL") (executable-find (getenv "SHELL")))
      (executable-find "bash")
      (executable-find "zsh")
      (executable-find "sh")
      (user-error "popup-term: could not locate a shell executable")))

;;;; --- core -----------------------------------------------------------------
(defun popup-term-open ()
  "Open a terminal in a Popper popup rooted at the current buffer’s dir."
  (interactive)
  (let* ((default-directory
          (or (and (buffer-file-name)
                   (file-name-directory (buffer-file-name)))
              default-directory))
         (shell  (popup-term--find-shell))
         (buffer (generate-new-buffer "*ansi-term*")))
    ;; 1) create the subprocess *without* displaying the buffer
    (with-current-buffer buffer
      (term-mode)
      (term-exec buffer
                 (file-name-nondirectory shell) shell nil nil)
      (term-char-mode))
    ;; 2) show it via Popper
    (pop-to-buffer buffer)))

;;;; --- key binding ----------------------------------------------------------
(global-set-key (kbd "C-c t") #'popup-term-open)

(provide 'popup-term)
;;; popup-term.el ends here
