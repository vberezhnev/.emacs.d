;;; xcscope-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "xcscope" "xcscope.el" (0 0 0 0))
;;; Generated autoloads from xcscope.el

(autoload 'cscope-minor-mode "xcscope" "\
This cscope minor mode maps cscope keybindings to make cscope
functions more accessible.

This is a minor mode.  If called interactively, toggle the
`Cscope minor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `cscope-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Key bindings:
\\{cscope-minor-mode-keymap}

\(fn &optional ARG)" t nil)

(autoload 'cscope-setup "xcscope" "\
Automatically turns on cscope-minor-mode when editing C and
C++ sources" t nil)

(register-definition-prefixes "xcscope" '("cscope-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; xcscope-autoloads.el ends here
