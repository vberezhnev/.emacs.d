;;; company.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Your Name
;;
;; Author: Your Name <your.email@example.com>
;; Maintainer: Your Name <your.email@example.com>
;; Created: June 15, 2025
;; Modified: June 15, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/berezhnev/company
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;; Company Configuration
(use-package company
  :straight t
  :bind (("C-c ." . company-complete)
         ;; ("C-c C-." . company-complete)
         ;; ("C-c s s" . company-yasnippet)
         :map company-active-map
         ("C-n" . company-select-next)
     ("C-p" . company-select-previous)
     ("C-d" . company-show-doc-buffer)
     ("M-." . company-show-location))
  ;; :hook (prog-mode . global-company-mode)
  :config
  ;; (eval-after-load 'rustic-mode
  ;;   '(define-key rustic-mode-map (kbd "[tab]") 'company-complete))

  ;; (setq company-tooltip-limit 8)
  ;; (setq company-show-numbers t)
  ;; (setq company-dabbrev-downcase nil)
  ;; (setq company-idle-delay 0)
  ;; (setq company-echo-delay 0)
  ;; ;; (setq company-ispell-dictionary (f-join tychoish-config-path "aspell-pws"))
  
  ;; (setq company-backends '(company-capf
  ;;              company-keywords
  ;;              company-semantic
  ;;              company-files
  ;;              company-etags
  ;;              company-elisp
  ;;              company-clang
  ;;              company-irony-c-headers
  ;;              company-irony
  ;;              ;; company-jedi
  ;;              company-cmake
  ;;              company-ispell
  ;;              company-yasnippet))

  (global-company-mode))

(use-package company-quickhelp
  :after company
  :config
  (setq company-quickhelp-idle-delay 0.1)
  (company-quickhelp-mode 1))

(use-package company-irony
  :straight t
  :after (company irony)
  :commands (company-irony)
  :config
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

(use-package company-irony-c-headers
  :straight t
  :commands (company-irony-c-headers)
  :after company-irony)

(use-package company-jedi
  :straight t
  :commands (company-jedi)
  :after (company python-mode))

(use-package company-statistics
  :straight t
  :after company
  :config
  (company-statistics-mode))

(use-package nerd-icons
  :straight t)

(use-package nerd-icons-corfu
  :straight t)

(use-package nerd-icons-dired
  :straight t)

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Company-box: Enhanced UI for company completions
;; (use-package company-box
;;   :straight t
;;   :hook (company-mode . company-box-mode) ; Enable when company-mode is active
;;   :config
;;   (setq ;;company-box-show-single-candidate t
;;         ;; company-box-backends-colors t
;;         company-box-tooltip-limit 50
;;         ;; company-box-icons-alist 'company-box-icons-nerd-icons
;;         ;; company-box-icons-functions
;;         ;; (cons #'+company-box-icons--elisp-fn
;;         ;;       (delq 'company-box-icons--elisp company-box-icons-functions))
;;         ;; company-box-icons-nerd-icons
;;         ;; `((Unknown        . ,(nerd-icons-codicon "nf-cod-code" :face 'font-lock-warning-face))
;;         ;;   (Text           . ,(nerd-icons-codicon "nf-cod-text_size" :face 'font-lock-doc-face))
;;         ;;   (Method         . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'font-lock-function-name-face))
;;         ;;   (Function       . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'font-lock-function-name-face))
;;         ;;   (Constructor    . ,(nerd-icons-codicon "nf-cod-triangle_right" :face 'font-lock-function-name-face))
;;         ;;   (Field          . ,(nerd-icons-codicon "nf-cod-symbol_field" :face 'font-lock-variable-name-face))
;;         ;;   (Variable       . ,(nerd-icons-codicon "nf-cod-symbol_variable" :face 'font-lock-variable-name-face))
;;         ;;   (Class          . ,(nerd-icons-codicon "nf-cod-symbol_class" :face 'font-lock-type-face))
;;         ;;   (Interface      . ,(nerd-icons-codicon "nf-cod-symbol_interface" :face 'font-lock-type-face))
;;         ;;   (Module         . ,(nerd-icons-codicon "nf-cod-file_submodule" :face 'font-lock-preprocessor-face))
;;         ;;   (Property       . ,(nerd-icons-codicon "nf-cod-symbol_property" :face 'font-lock-variable-name-face))
;;         ;;   (Unit           . ,(nerd-icons-codicon "nf-cod-symbol_ruler" :face 'font-lock-constant-face))
;;         ;;   (Value          . ,(nerd-icons-codicon "nf-cod-symbol_field" :face 'font-lock-builtin-face))
;;         ;;   (Enum           . ,(nerd-icons-codicon "nf-cod-symbol_enum" :face 'font-lock-builtin-face))
;;         ;;   (Keyword        . ,(nerd-icons-codicon "nf-cod-symbol_keyword" :face 'font-lock-keyword-face))
;;         ;;   (Snippet        . ,(nerd-icons-codicon "nf-cod-symbol_snippet" :face 'font-lock-string-face))
;;         ;;   (Color          . ,(nerd-icons-codicon "nf-cod-symbol_color" :face 'success))
;;         ;;   (File           . ,(nerd-icons-codicon "nf-cod-symbol_file" :face 'font-lock-string-face))
;;         ;;   (Reference      . ,(nerd-icons-codicon "nf-cod-references" :face 'font-lock-variable-name-face))
;;         ;;   (Folder         . ,(nerd-icons-codicon "nf-cod-folder" :face 'font-lock-variable-name-face))
;;         ;;   (EnumMember     . ,(nerd-icons-codicon "nf-cod-symbol_enum_member" :face 'font-lock-builtin-face))
;;         ;;   (Constant       . ,(nerd-icons-codicon "nf-cod-symbol_constant" :face 'font-lock-constant-face))
;;         ;;   (Struct         . ,(nerd-icons-codicon "nf-cod-symbol_structure" :face 'font-lock-variable-name-face))
;;         ;;   (Event          . ,(nerd-icons-codicon "nf-cod-symbol_event" :face 'font-lock-warning-face))
;;         ;;   (Operator       . ,(nerd-icons-codicon "nf-cod-symbol_operator" :face 'font-lock-comment-delimiter-face))
;;         ;;   (TypeParameter  . ,(nerd-icons-codicon "nf-cod-list_unordered" :face 'font-lock-type-face))
;;         ;;   (Template       . ,(nerd-icons-codicon "nf-cod-symbol_snippet" :face 'font-lock-string-face))
;;         ;;   (ElispFunction  . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'font-lock-function-name-face))
;;         ;;   (ElispVariable  . ,(nerd-icons-codicon "nf-cod-symbol_variable" :face 'font-lock-variable-name-face))
;;         ;;   (ElispFeature   . ,(nerd-icons-codicon "nf-cod-globe" :face 'font-lock-builtin-face))
;;         ;;   (ElispFace      . ,(nerd-icons-codicon "nf-cod-symbol_color" :face 'success)))
;; 	)

;;   ;; Fix oversized scrollbar (Doom-specific hack, adapted for vanilla Emacs)
;;   (setq x-gtk-resize-child-frames 'resize-mode)

;;   ;; Disable tab-bar in company-box child frames
;;   ;; (add-to-list 'company-box-frame-parameters '(tab-bar-lines . 0))

;;   ;; Remove company-echo-metadata-frontend since company-box shows docs
;;   ;; (setq company-frontends
;;   ;;       (delq 'company-echo-metadata-frontend company-frontends))

;;   ;; Custom icon function for Emacs Lisp
;;   ;; (defun +company-box-icons--elisp-fn (candidate)
;;   ;;   (when (derived-mode-p 'emacs-lisp-mode)
;;   ;;     (let ((sym (intern candidate)))
;;   ;;       (cond ((fboundp sym) 'ElispFunction)
;;   ;;             ((boundp sym) 'ElispVariable)
;;   ;;             ((featurep sym) 'ElispFeature)
;;   ;;             ((facep sym) 'ElispFace)))))

;;   ;; Fix deleted frame detection
;;   ;; (defun +company-box-detect-deleted-frame-advice (frame)
;;   ;;   (if (frame-live-p frame) frame))
;;   ;; (advice-add 'company-box--get-frame :filter-return #'+company-box-detect-deleted-frame-advice)

;;   ;; (defun +company-box-detect-deleted-doc-frame-advice (_selection frame)
;;   ;;   (when (and company-box-doc-enable
;;   ;;              (frame-local-getq company-box-doc-frame frame)
;;   ;;              (not (frame-live-p (frame-local-getq company-box-doc-frame frame))))
;;   ;;     (frame-local-setq company-box-doc-frame nil frame)))
;;   ;; (advice-add 'company-box-doc :before #'+company-box-detect-deleted-doc-frame-advice))
;; )
  
;; Company-dict: Per-project dictionaries
;; (use-package company-dict
;;   :straight t
;;   :defer t
;;   :config
;;   (setq company-dict-dir (expand-file-name "dicts" user-emacs-directory))
;;   (defun +company-enable-project-dicts (mode &rest _)
;;     "Enable per-project dictionaries for the given MODE."
;;     (if (symbol-value mode)
;;         (add-to-list 'company-dict-minor-mode-list mode nil #'eq)
;;       (setq company-dict-minor-mode-list
;;             (delq mode company-dict-minor-mode-list))))
;;   (add-hook 'projectile-mode-hook #'+company-enable-project-dicts))

;;;;;;;;;;;;;;;;;;;;;;; COMPANY-ORG-BLOCK ;;;;;;;;;;;;;;;;;;;;;;;

(use-package company-org-block
:straight t
:defer t
:custom
(company-org-block-edit-style 'auto)
:hook ((org-mode . (lambda ()
                     (setq-local company-backends '(company-org-block))
                     (company-mode +1)))))


(provide 'company)
;;; company.el ends here
