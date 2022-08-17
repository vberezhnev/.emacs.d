(load "~/.emacs.d/setting-packages")
(load "~/.emacs.d/setting-lsp")
(load "~/.emacs.d/setting-font-face")

(setq make-backup-files nil)          ; Delete #filename# files

(setq-default message-log-max nil)
(kill-buffer "*Messages*")

(add-hook 'minibuffer-exit-hook
	'(lambda ()
           (let ((buffer "*Completions*"))
             (and (get-buffer buffer)
	  	(kill-buffer buffer)))))

(setq initial-major-mode (quote fundamental-mode))

(setq display-line-numbers-type t)

(xterm-mouse-mode 1)

(setq-default tab-width 2) ; set default tab char's display width to 2 spaces
(setq tab-width 2)         ; set current buffer's tab char's display width to 2 spaces

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
      (lambda ()
        (setq gc-cons-threshold (expt 2 23))))
