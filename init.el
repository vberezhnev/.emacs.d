(require 'package)
; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t) 
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
	("ORG"		. "https://orgmode.org/elpa/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("ORG"		. 20)
	("MELPA"        . 15)
	("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)))

;; (add-to-list 'package-archives '("melpa"  . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
;; (setq package-archive-priorities '(("melpa"  . 3)
;;                                    ("gnu"    . 2)
;;                                    ("nongnu" . 1)))
(package-initialize)

(setq debug-on-error t)

;;(package-refresh-contents)

;; (save-excursion
;;   (let ((tem eval-buffer-list))
;;     (while (and tem
;;                 (re-search-forward "^  eval-\\(buffer\\|region\\)(" nil t))
;;       (beginning-of-line)
;;       (insert (apply 'format "Error at line %d, column %d (point %d) in %s\n"
;;                      (with-current-buffer (car tem)
;;                        (list (line-number-at-pos (point))
;;                              (current-column)
;;                              (point)
;;                              (buffer-name)))))
;;       (pop tem))))

;; (with-eval-after-load 'debug
;;   (defun debugger-setup-buffer (debugger-args)
;;     "Initialize the `*Backtrace*' buffer for entry to the debugger.
;; That buffer should be current already."
;;     (setq buffer-read-only nil)
;;     (erase-buffer)
;;     (set-buffer-multibyte t)        ;Why was it nil ?  -stef
;;     (setq buffer-undo-list t)
;;     (let ((standard-output (current-buffer))
;;           (print-escape-newlines t)
;;           (print-level 8)
;;           (print-length 50))
;;       (backtrace))
;;     (goto-char (point-min))
;;     (delete-region (point)
;;                    (progn
;;                      (search-forward "\n  debug(")
;;                      (forward-line (if (eq (car debugger-args) 'debug)
;;                                        2    ; Remove implement-debug-on-entry frame.
;;                                      1))
;;                      (point)))
;;     (insert "Debugger entered")
;;     ;; lambda is for debug-on-call when a function call is next.
;;     ;; debug is for debug-on-entry function called.
;;     (pcase (car debugger-args)
;;       ((or `lambda `debug)
;;        (insert "--entering a function:\n"))
;;       ;; Exiting a function.
;;       (`exit
;;        (insert "--returning value: ")
;;        (setq debugger-value (nth 1 debugger-args))
;;        (prin1 debugger-value (current-buffer))
;;        (insert ?\n)
;;        (delete-char 1)
;;        (insert ? )
;;        (beginning-of-line))
;;       ;; Debugger entered for an error.
;;       (`error
;;        (insert "--Lisp error: ")
;;        (prin1 (nth 1 debugger-args) (current-buffer))
;;        (insert ?\n))
;;       ;; debug-on-call, when the next thing is an eval.
;;       (`t
;;        (insert "--beginning evaluation of function call form:\n"))
;;       ;; User calls debug directly.
;;       (_
;;        (insert ": ")
;;        (prin1 (if (eq (car debugger-args) 'nil)
;;                   (cdr debugger-args) debugger-args)
;;               (current-buffer))
;;        (insert ?\n)))
;;     ;; After any frame that uses eval-buffer,
;;     ;; insert a line that states the buffer position it's reading at.
;;     (save-excursion
;;       (let ((tem eval-buffer-list))
;;         (while (and tem
;;                     (re-search-forward "^  eval-\\(buffer\\|region\\)(" nil t))
;;           (beginning-of-line)
;;           (insert (format "Error at line %d in %s: "
;;                           (with-current-buffer (car tem)
;;                             (line-number-at-pos (point)))
;;                           (with-current-buffer (car tem)
;;                             (buffer-name))))
;;           (pop tem))))
;;     (debugger-make-xrefs)))

;; Install use-package
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'org)

(org-babel-load-file
 (expand-file-name
  "README.org"
  user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(before-save-hook '(parrot-start-animation format-all-buffer))
 '(custom-safe-themes
	 '("afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "2ff9ac386eac4dffd77a33e93b0c8236bb376c5a5df62e36d4bfa821d56e4e20" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "19a2c0b92a6aa1580f1be2deb7b8a8e3a4857b6c6ccf522d00547878837267e7" "3e374bb5eb46eb59dbd92578cae54b16de138bc2e8a31a2451bf6fdb0f3fd81b" "" default))
 '(doom-modeline-bar-width 6)
 '(doom-modeline-buffer-file-name-style 'auto)
 '(doom-modeline-buffer-name nil)
 '(doom-modeline-buffer-state-icon t)
 '(doom-modeline-height 30)
 '(doom-modeline-highlight-modified-buffer-name nil)
 '(doom-modeline-icon t)
 '(doom-modeline-project-detection 'projectile)
 '(global-wakatime-mode t)
 '(helm-minibuffer-history-key "M-p")
 '(magit-todos-keywords (list "TODO" "FIXME" "HACK"))
 '(org-safe-remote-resources
	 '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'" "\\`https://fniessen\\.github\\.io\\(?:/\\|\\'\\)"))
 '(package-selected-packages
	 '(latex-preview-pane auctex-latexmk auctex lorem-ipsum neotree zygospore xwwp xwidgets-reuse xwidgete xml+ xclip which-key web-mode use-package treemacs-evil treemacs-all-the-icons tree-sitter-langs tide telega saveplace-pdf-view rust-playground rust-auto-use reverse-im ranger rainbow-delimiters racer projectile pdf-view-restore pbcopy parrot org-superstar org-modern org-caldav org-alert nov names multi-vterm multi-term lsp-ui json-mode js2-mode indent-guide import-js helm gruvbox-theme graphql go-mode git-gutter-fringe general fzf format-all flycheck-rust flycheck-inline evil-collection emojify elfeed-dashboard doom-themes doom-modeline dmenu djvu dired-subtree dashboard dap-mode company-statistics company-quickhelp company-anaconda cl-libify cargo calibredb atom-one-dark-theme apheleia all-the-icons-dired))
 '(warning-suppress-log-types '(((org-roam)) ((org-roam)) (comp)))
 '(warning-suppress-types '(((org-roam)) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face ((t :foreground "#E46876" :height 140 :italic t)) t)
 '(doom-modeline-time ((t (:inherit (mode-line-buffer-id bold) :box (:line-width (2 . 2) :color "dim gray" :style flat-button))))))
(put 'dired-find-alternate-file 'disabled nil)
