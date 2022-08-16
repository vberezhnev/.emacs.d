;;; apheleia-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "apheleia" "apheleia.el" (0 0 0 0))
;;; Generated autoloads from apheleia.el

(autoload 'apheleia-format-buffer "apheleia" "\
Run code formatter asynchronously on current buffer, preserving point.

FORMATTER is a symbol appearing as a key in
`apheleia-formatters', or a list of them to run multiple
formatters in a chain. If called interactively, run the currently
configured formatters (see `apheleia-formatter' and
`apheleia-mode-alist'), or prompt from `apheleia-formatters' if
there is none configured for the current buffer. With a prefix
argument, prompt always.

After the formatters finish running, the diff utility is invoked to
determine what changes it made. That diff is then used to apply the
formatter's changes to the current buffer without moving point or
changing the scroll position in any window displaying the buffer. If
the buffer has been modified since the formatter started running,
however, the operation is aborted.

If the formatter actually finishes running and the buffer is
successfully updated (even if the formatter has not made any
changes), CALLBACK, if provided, is invoked with no arguments.

\(fn FORMATTER &optional CALLBACK)" t nil)

(autoload 'apheleia--format-after-save "apheleia" "\
Run code formatter for current buffer if any configured, then save." nil nil)

(define-minor-mode apheleia-mode "\
Minor mode for reformatting code on save without moving point.
It is customized by means of the variables `apheleia-mode-alist'
and `apheleia-formatters'." :lighter " Apheleia" (if apheleia-mode (add-hook 'after-save-hook #'apheleia--format-after-save nil 'local) (remove-hook 'after-save-hook #'apheleia--format-after-save 'local)))

(define-globalized-minor-mode apheleia-global-mode apheleia-mode apheleia-mode)

(put 'apheleia-mode 'safe-local-variable #'booleanp)

(register-definition-prefixes "apheleia" '("apheleia-"))

;;;***

;;;### (autoloads nil nil ("apheleia-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; apheleia-autoloads.el ends here
