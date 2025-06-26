;;; twitch-dark-theme.el --- Minimal "Twitch Dark" theme  -*- lexical-binding: t; -*-
;; Author: send0xx <you@example.com>
;; URL: https://github.com/send0xx/twitch-dark-theme
;; Version: 0.7
;; Keywords: faces, theme, dark, rust
;; Package-Requires: ((emacs "24.3"))
;;
;; Twitch‑flavoured dark theme (matching JetBrains *Twitch Dark*)
;; focused on core UI & Rust.

;;; Code:

(deftheme twitch-dark "Twitch Dark Theme (minimalist).")

;; ---------------------------------------------------------------------------
;; Palette
;; ---------------------------------------------------------------------------
(defconst twitch-dark-bg        "#161617")
(defconst twitch-dark-bg-alt    "#232324")
(defconst twitch-dark-hl        "#343436")
(defconst twitch-dark-fg        "#dedee3")
(defconst twitch-dark-var-fg    "#e6e6ea")
(defconst twitch-dark-fg-dim    "#b0b0b8")
(defconst twitch-dark-fg-dis    "#686a4e")
(defconst twitch-dark-purple    "#9147ff")
(defconst twitch-dark-purple-soft "#5c16c5")
(defconst twitch-dark-keyword   "#b48fea")
(defconst twitch-dark-orange    "#ffa656")
(defconst twitch-dark-wheat     "#e5c07b")
(defconst twitch-dark-teal      "#4eade5")
(defconst twitch-dark-param     "#20999d")
(defconst twitch-dark-string    "#e09f63")
(defconst twitch-dark-warning   "#d0e94c")

(defun twitch-dark--spec (props &optional tty-props)
  `((((min-colors 16777216)) ,props)
    (((min-colors 256))      ,(or tty-props props))))

;; ---------------------------------------------------------------------------
;; Faces
;; ---------------------------------------------------------------------------
(custom-theme-set-faces
 'twitch-dark
 ;; --- core UI ------------------------------------------------------------
 `(default                       ,(twitch-dark--spec `(:background ,twitch-dark-bg :foreground ,twitch-dark-fg)))
 `(cursor                        ,(twitch-dark--spec `(:background ,twitch-dark-purple)))
 `(region                        ,(twitch-dark--spec `(:background ,twitch-dark-hl)))
 `(highlight                     ,(twitch-dark--spec `(:background ,twitch-dark-hl)))
 `(fringe                        ,(twitch-dark--spec `(:background ,twitch-dark-bg :foreground ,twitch-dark-fg-dim)))
 `(minibuffer-prompt             ,(twitch-dark--spec `(:foreground ,twitch-dark-purple :weight bold)))
 `(mode-line                     ,(twitch-dark--spec `(:background ,twitch-dark-bg-alt :foreground ,twitch-dark-fg
                                         :box (:line-width -1 :color ,twitch-dark-bg-alt))))
 `(mode-line-inactive            ,(twitch-dark--spec `(:background ,twitch-dark-bg :foreground ,twitch-dark-fg-dim
                                         :box (:line-width -1 :color ,twitch-dark-bg))))
 `(link                          ,(twitch-dark--spec `(:foreground ,twitch-dark-purple :underline t)))
 `(isearch                       ,(twitch-dark--spec `(:background ,twitch-dark-purple-soft :foreground ,twitch-dark-bg)))
 `(lazy-highlight                ,(twitch-dark--spec `(:background ,twitch-dark-bg-alt)))
 `(show-paren-match              ,(twitch-dark--spec `(:foreground ,twitch-dark-purple :weight bold)))
 `(show-paren-mismatch           ,(twitch-dark--spec `(:background ,twitch-dark-purple :foreground ,twitch-dark-bg)))
 `(trailing-whitespace           ,(twitch-dark--spec `(:background ,twitch-dark-purple-soft)))
 `(rust-inactive-code-face       ,(twitch-dark--spec `(:foreground ,twitch-dark-fg-dis)))
 `(shadow                        ,(twitch-dark--spec `(:foreground ,twitch-dark-fg-dis)))
 ;; --- tabs ---------------------------------------------------------------
 `(tab-bar                       ,(twitch-dark--spec `(:background ,twitch-dark-bg-alt :foreground ,twitch-dark-fg-dim)))
 `(tab-bar-tab                   ,(twitch-dark--spec `(:background ,twitch-dark-bg :foreground ,twitch-dark-wheat
                                         :box (:line-width 2 :color ,twitch-dark-bg) :weight bold)))
 `(tab-bar-tab-inactive          ,(twitch-dark--spec `(:background ,twitch-dark-bg-alt :foreground ,twitch-dark-wheat
                                         :box (:line-width 2 :color ,twitch-dark-bg-alt))))
 `(tab-line                      ,(twitch-dark--spec `(:background ,twitch-dark-bg-alt :foreground ,twitch-dark-fg-dim)))
 `(tab-line-tab-current          ,(twitch-dark--spec `(:background ,twitch-dark-bg :foreground ,twitch-dark-wheat
                                         :box (:line-width 4 :color ,twitch-dark-bg) :weight bold)))
 `(tab-line-tab-inactive         ,(twitch-dark--spec `(:background ,twitch-dark-bg-alt :foreground ,twitch-dark-wheat
                                         :box (:line-width 4 :color ,twitch-dark-bg-alt))))
 ;; --- syntax -------------------------------------------------------------
 `(font-lock-comment-face        ,(twitch-dark--spec `(:foreground ,twitch-dark-fg-dim)))
 `(font-lock-comment-delimiter-face ,(twitch-dark--spec `(:foreground ,twitch-dark-fg-dim)))
 `(font-lock-doc-face            ,(twitch-dark--spec `(:foreground ,twitch-dark-fg-dim)))
 `(font-lock-keyword-face        ,(twitch-dark--spec `(:foreground ,twitch-dark-keyword)))
 `(font-lock-function-name-face  ,(twitch-dark--spec `(:foreground ,twitch-dark-fg)))
 `(font-lock-type-face           ,(twitch-dark--spec `(:foreground ,twitch-dark-wheat)))
 `(font-lock-constant-face       ,(twitch-dark--spec `(:foreground ,twitch-dark-wheat)))
 `(font-lock-variable-name-face  ,(twitch-dark--spec `(:foreground ,twitch-dark-var-fg)))
 `(font-lock-builtin-face        ,(twitch-dark--spec `(:foreground ,twitch-dark-teal)))
 `(font-lock-string-face         ,(twitch-dark--spec `(:foreground ,twitch-dark-string)))
 `(font-lock-number-face         ,(twitch-dark--spec `(:foreground ,twitch-dark-wheat)))
 `(font-lock-warning-face        ,(twitch-dark--spec `(:foreground ,twitch-dark-warning :weight bold)))
 ;; --- rust-mode extras ---------------------------------------------------
 `(rust-attribute-face           ,(twitch-dark--spec `(:foreground ,twitch-dark-keyword)))
 `(rust-builtin-formatting-macro-face ,(twitch-dark--spec `(:foreground ,twitch-dark-teal)))
 `(rust-macro-face               ,(twitch-dark--spec `(:foreground ,twitch-dark-teal)))
 `(rust-unsafe-face              ,(twitch-dark--spec `(:foreground ,twitch-dark-warning :weight bold)))
 `(rust-question-mark-face       ,(twitch-dark--spec `(:foreground ,twitch-dark-purple)))
 `(rust-string-interpolation-face ,(twitch-dark--spec `(:foreground ,twitch-dark-string :slant italic)))
 `(rust-lifetime-face            ,(twitch-dark--spec `(:foreground ,twitch-dark-param)))
 `(rust-self-face                ,(twitch-dark--spec `(:foreground ,twitch-dark-var-fg :weight bold)))
 `(rust-ty-param-face            ,(twitch-dark--spec `(:foreground ,twitch-dark-param)))
;; --- LSP breadcrumb bar -------------------------------------------------
`(header-line
  ,(twitch-dark--spec `(:background ,twitch-dark-bg-alt :foreground ,twitch-dark-fg)))

`(lsp-headerline-breadcrumb-path-face
  ,(twitch-dark--spec `(:background ,twitch-dark-bg-alt :foreground ,twitch-dark-wheat :weight bold)))

`(lsp-headerline-breadcrumb-separator-face
  ,(twitch-dark--spec `(:background ,twitch-dark-bg-alt :foreground ,twitch-dark-fg-dis)))

`(lsp-headerline-breadcrumb-file-face
  ,(twitch-dark--spec `(:background ,twitch-dark-bg-alt :foreground ,twitch-dark-wheat)))

`(lsp-headerline-breadcrumb-project-prefix-face
  ,(twitch-dark--spec `(:background ,twitch-dark-bg-alt :foreground ,twitch-dark-wheat)))

`(lsp-headerline-breadcrumb-symbols-face
  ,(twitch-dark--spec `(:background ,twitch-dark-bg-alt :foreground ,twitch-dark-wheat)))
)
;; ---------------------------------------------------------------------------
;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'twitch-dark)
;;; twitch-dark-theme.el ends here

