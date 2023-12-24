
;; ;; https://gitlab.com/shilling.jake/emacsd/-/blob/master/config.org
;; (setq gc-cons-threshold most-positive-fixnum)

;; (defconst 1mb 1048576)
;; (defconst 20mb 20971520)
;; (defconst 30mb 31457280)
;; (defconst 50mb 52428800)

;; (defun fk/defer-garbage-collection ()
;;   (setq gc-cons-threshold most-positive-fixnum))

;; (defun fk/restore-garbage-collection ()
;;   (run-at-time 1 nil (lambda () (setq gc-cons-threshold 30mb))))

;; (add-hook 'emacs-startup-hook 'fk/restore-garbage-collection)
;; (add-hook 'minibuffer-setup-hook 'fk/defer-garbage-collection)
;; (add-hook 'minibuffer-exit-hook 'fk/restore-garbage-collection)

;; (setq read-process-output-max (* 3 1mb))  ;; lsp-mode's performance suggest

;; (defvar default-file-name-handler-alist file-name-handler-alist)
;; (setq file-name-handler-alist nil)

;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq file-name-handler-alist default-file-name-handler-alist)))

;; (setq frame-inhibit-implied-resize t)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;;;;;;;;;;;;;;;;;;;;;;; ;;
;; ;; ;; LSP optimizations ;; ;;
;; ;; ;;;;;;;;;;;;;;;;;;;;;;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'cl-lib)
;; (cl-declaim (optimize (speed 3) (safety 0)))
;; (cl-defstruct my/struct counter)

;; (define-inline test-counter-inc (self)
;;   (inline-letevals (self)
;;     (inline-quote
;;      (setf (my/struct-counter ,self) (1+ (my/struct-counter ,self))))))

;; (require 'cl-lib)
;; (cl-declaim (optimize (speed 3) (safety 0)))
;; (cl-defstruct my/struct counter)

;; (define-inline test-counter-inc (self)
;;   (inline-letevals (self)
;;     (inline-quote
;;      (setf (my/struct-counter ,self) (1+ (my/struct-counter ,self))))))

;; (setq read-process-output-max (* 1024 1024))

;; ;; Doom emacs:
;; (defvar +lsp--default-read-process-output-max nil)
;; (defvar +lsp--default-gcmh-high-cons-threshold nil)
;; (defvar +lsp--optimization-init-p nil)

;; (define-minor-mode +lsp-optimization-mode
;;   "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
;;   :global t
;;   :init-value nil
;;   (if (not +lsp-optimization-mode)
;;       (setq-default read-process-output-max +lsp--default-read-process-output-max
;;                     gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
;;                     +lsp--optimization-init-p nil)
;;     ;; Only apply these settings once!
;;     (unless +lsp--optimization-init-p
;;       (setq +lsp--default-read-process-output-max (default-value 'read-process-output-max)
;;             +lsp--default-gcmh-high-cons-threshold (default-value 'gcmh-high-cons-threshold))
;;       (setq-default read-process-output-max (* 1024 1024))
;;       ;; REVIEW LSP causes a lot of allocations, with or without the native JSON
;;       ;;        library, so we up the GC threshold to stave off GC-induced
;;       ;;        slowdowns/freezes. Doom uses `gcmh' to enforce its GC strategy,
;;       ;;        so we modify its variables rather than `gc-cons-threshold'
;;       ;;        directly.
;;       (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
;;       (gcmh-set-high-threshold)
;;       (setq +lsp--optimization-init-p t))))

