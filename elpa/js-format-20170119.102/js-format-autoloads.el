;;; js-format-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "js-format" "js-format.el" (0 0 0 0))
;;; Generated autoloads from js-format.el

(autoload 'js-format-mark-statement "js-format" "\
Mark js statement at point.
Will avoid mark non-formattable node when SKIP-NON-STATEMENT is non-nil.

\(fn &optional SKIP-NON-STATEMENT)" t nil)

(autoload 'js-format-buffer "js-format" "\
Format current buffer." t nil)

(autoload 'js-format-line "js-format" "\
Format line before point." t nil)

(autoload 'js-format-region "js-format" "\
Format region between START and END.
When NOT-JUMP-P is non-nil, won't jump to error position when format error.
POS-LIST is list of (line column) to restore point after format.

\(fn START END &optional NOT-JUMP-P POS-LIST)" t nil)

(autoload 'js-format-setup "js-format" "\
Switch to and setup the active format style to STYLE.
If STYLE changed, will call the style's setup command to setup.
If with C-u, will prompt to set `js-format-server'.
RETURN the current active style.

\(fn &optional STYLE SERVER)" t nil)

(autoload 'js-format-exit "js-format" "\
Exit js-format node server." t nil)

(register-definition-prefixes "js-format" '("js-format-"))

;;;***

;;;### (autoloads nil nil ("js-format-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; js-format-autoloads.el ends here
