;;; simple-modeline-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "simple-modeline" "simple-modeline.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from simple-modeline.el

(defvar simple-modeline-mode nil "\
Non-nil if Simple-Modeline mode is enabled.
See the `simple-modeline-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `simple-modeline-mode'.")

(custom-autoload 'simple-modeline-mode "simple-modeline" nil)

(autoload 'simple-modeline-mode "simple-modeline" "\
Minor mode to get a simple mode line.

When called interactively, toggle
`simple-modeline-mode'.  With prefix ARG, enable
`simple-modeline--mode' if ARG is positive, otherwise
disable it.

When called from Lisp, enable `simple-modeline-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `simple-modeline-mode'.
Otherwise behave as if called interactively.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "simple-modeline" '("simple-modeline--mode-line"))

;;;***

;;;### (autoloads nil "simple-modeline-core" "simple-modeline-core.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from simple-modeline-core.el

(register-definition-prefixes "simple-modeline-core" '("simple-modeline-"))

;;;***

;;;### (autoloads nil "simple-modeline-segments" "simple-modeline-segments.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from simple-modeline-segments.el

(register-definition-prefixes "simple-modeline-segments" '("simple-modeline-"))

;;;***

;;;### (autoloads nil nil ("simple-modeline-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; simple-modeline-autoloads.el ends here
