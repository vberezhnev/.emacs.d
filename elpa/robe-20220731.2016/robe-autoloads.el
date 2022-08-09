;;; robe-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ac-robe" "ac-robe.el" (0 0 0 0))
;;; Generated autoloads from ac-robe.el

(autoload 'ac-robe-available "ac-robe" "\
Return t if `robe-mode' completions are available, otherwise nil." nil nil)

(autoload 'ac-robe-setup "ac-robe" nil nil nil)

(defconst ac-source-robe '((available . ac-robe-available) (prefix . ac-robe-prefix) (candidates . ac-robe-candidates) (document . ac-robe-doc) (symbol . "r")) "\
`auto-complete' completion source for Ruby using `robe-mode'.")

(register-definition-prefixes "ac-robe" '("ac-robe-"))

;;;***

;;;### (autoloads nil "company-robe" "company-robe.el" (0 0 0 0))
;;; Generated autoloads from company-robe.el

(autoload 'company-robe "company-robe" "\
A `company-mode' completion back-end for `robe-mode'.

\(fn COMMAND &optional ARG &rest IGNORE)" t nil)

(register-definition-prefixes "company-robe" '("company-robe--"))

;;;***

;;;### (autoloads nil "robe" "robe.el" (0 0 0 0))
;;; Generated autoloads from robe.el

(autoload 'robe-mode "robe" "\
Improved navigation for Ruby.

This is a minor mode.  If called interactively, toggle the `robe
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `robe-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

The following commands are available:

\\{robe-mode-map}

\(fn &optional ARG)" t nil)

(put 'global-robe-mode 'globalized-minor-mode t)

(defvar global-robe-mode nil "\
Non-nil if Global Robe mode is enabled.
See the `global-robe-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-robe-mode'.")

(custom-autoload 'global-robe-mode "robe" nil)

(autoload 'global-robe-mode "robe" "\
Toggle Robe mode in all buffers.
With prefix ARG, enable Global Robe mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Robe mode is enabled in all buffers where `robe-mode-on' would do it.

See `robe-mode' for more information on Robe mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "robe" '("robe-"))

;;;***

;;;### (autoloads nil nil ("robe-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; robe-autoloads.el ends here
