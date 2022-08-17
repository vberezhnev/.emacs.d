;;; xah-fly-keys-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "xah-fly-keys" "xah-fly-keys.el" (0 0 0 0))
;;; Generated autoloads from xah-fly-keys.el

(defvar xah-fly-keys nil "\
Non-nil if Xah-Fly-Keys mode is enabled.
See the `xah-fly-keys' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `xah-fly-keys'.")

(custom-autoload 'xah-fly-keys "xah-fly-keys" nil)

(autoload 'xah-fly-keys "xah-fly-keys" "\
A modal keybinding set, like vim, but based on ergonomic
  principles, like Dvorak layout.

This is a minor mode.  If called interactively, toggle the
`Xah-Fly-Keys mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='xah-fly-keys)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

URL `http://xahlee.info/emacs/misc/xah-fly-keys.html'

\(fn &optional ARG)" t nil)

(register-definition-prefixes "xah-fly-keys" '("xah-"))

;;;***

;;;### (autoloads nil nil ("xah-fly-keys-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; xah-fly-keys-autoloads.el ends here
