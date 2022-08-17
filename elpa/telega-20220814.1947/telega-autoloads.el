;;; telega-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "telega" "telega.el" (0 0 0 0))
;;; Generated autoloads from telega.el

(defvar telega-prefix-map (let ((map (make-sparse-keymap))) (define-key map (kbd "a") 'telega-account-switch) (define-key map (kbd "b") 'telega-switch-buffer) (define-key map (kbd "c") 'telega-chat-with) (define-key map (kbd "e") 'telega-edit-file-switch-buffer) (define-key map (kbd "i") 'telega-switch-important-chat) (define-key map (kbd "f") 'telega-buffer-file-send) (define-key map (kbd "s") 'telega-saved-messages) (define-key map (kbd "t") 'telega) (define-key map (kbd "u") 'telega-switch-unread-chat) (define-key map (kbd "w") 'telega-browse-url) map) "\
Keymap for the telega commands.")

(autoload 'telega "telega" "\
Start telega.el Telegram client.
Pop to root buffer.
If `\\[universal-argument]' is specified, then do not pop to root buffer.

\(fn &optional ARG)" t nil)

(autoload 'telega-kill "telega" "\
Kill currently running telega.
With prefix arg FORCE quit without confirmation.

\(fn FORCE)" t nil)

(autoload 'telega-version "telega" "\
Return telega (and TDLib) version.
If `\\[universal-argument]' is specified, then insert the version
string at point.

\(fn &optional INSERT-P)" t nil)

(autoload 'telega-report-bug "telega" "\
Create bug report for https://github.com/zevlg/telega.el/issues." t nil)

(register-definition-prefixes "telega" '("telega-"))

;;;***

;;;### (autoloads nil "telega-chat" "telega-chat.el" (0 0 0 0))
;;; Generated autoloads from telega-chat.el

(autoload 'telega-chatbuf-input-as-region-advice "telega-chat" "\
Advice for commands accepting region.
If point is inside telega chatbuf input, then call region command
with input prompt as region.

\(fn ORIG-REGION-FUNC START END &rest ARGS)" nil nil)

(register-definition-prefixes "telega-chat" '("telega-" "with-telega-chatbuf-action"))

;;;***

;;;### (autoloads nil "telega-company" "telega-company.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from telega-company.el

(autoload 'telega-company-emoji "telega-company" "\
Backend for `company' to complete emojis.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'telega-company-telegram-emoji "telega-company" "\
Backend for `company' to complete emojis using `searchEmojis' TDLib method.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'telega-company-username "telega-company" "\
Backend for `company' to complete usernames.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'telega-company-hashtag "telega-company" "\
Backend for `company' to complete recent hashtags.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'telega-company-botcmd "telega-company" "\


\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "telega-company" '("telega-company-"))

;;;***

;;;### (autoloads nil "telega-core" "telega-core.el" (0 0 0 0))
;;; Generated autoloads from telega-core.el

(register-definition-prefixes "telega-core" '("telega-" "with-telega-"))

;;;***

;;;### (autoloads nil "telega-customize" "telega-customize.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from telega-customize.el

(register-definition-prefixes "telega-customize" '("telega-"))

;;;***

;;;### (autoloads nil "telega-ffplay" "telega-ffplay.el" (0 0 0 0))
;;; Generated autoloads from telega-ffplay.el

(register-definition-prefixes "telega-ffplay" '("telega-"))

;;;***

;;;### (autoloads nil "telega-filter" "telega-filter.el" (0 0 0 0))
;;; Generated autoloads from telega-filter.el

(register-definition-prefixes "telega-filter" '("telega-" "tracking-buffers"))

;;;***

;;;### (autoloads nil "telega-folders" "telega-folders.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from telega-folders.el

(register-definition-prefixes "telega-folders" '("telega-"))

;;;***

;;;### (autoloads nil "telega-i18n" "telega-i18n.el" (0 0 0 0))
;;; Generated autoloads from telega-i18n.el

(register-definition-prefixes "telega-i18n" '("telega-i18n"))

;;;***

;;;### (autoloads nil "telega-info" "telega-info.el" (0 0 0 0))
;;; Generated autoloads from telega-info.el

(register-definition-prefixes "telega-info" '("telega-"))

;;;***

;;;### (autoloads nil "telega-inline" "telega-inline.el" (0 0 0 0))
;;; Generated autoloads from telega-inline.el

(register-definition-prefixes "telega-inline" '("telega-"))

;;;***

;;;### (autoloads nil "telega-ins" "telega-ins.el" (0 0 0 0))
;;; Generated autoloads from telega-ins.el

(register-definition-prefixes "telega-ins" '("telega-"))

;;;***

;;;### (autoloads nil "telega-match" "telega-match.el" (0 0 0 0))
;;; Generated autoloads from telega-match.el

(register-definition-prefixes "telega-match" '("and" "call" "chat-" "define-telega-matcher" "ids" "msg-" "not" "prop" "return" "sender-" "telega-" "user-"))

;;;***

;;;### (autoloads nil "telega-media" "telega-media.el" (0 0 0 0))
;;; Generated autoloads from telega-media.el

(register-definition-prefixes "telega-media" '("telega-"))

;;;***

;;;### (autoloads nil "telega-modes" "telega-modes.el" (0 0 0 0))
;;; Generated autoloads from telega-modes.el

(defvar telega-mode-line-mode nil "\
Non-nil if Telega-Mode-Line mode is enabled.
See the `telega-mode-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `telega-mode-line-mode'.")

(custom-autoload 'telega-mode-line-mode "telega-modes" nil)

(autoload 'telega-mode-line-mode "telega-modes" "\
Toggle display of the unread chats/mentions in the modeline.

This is a minor mode.  If called interactively, toggle the
`Telega-Mode-Line mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='telega-mode-line-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(defvar telega-appindicator-mode nil "\
Non-nil if Telega-Appindicator mode is enabled.
See the `telega-appindicator-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `telega-appindicator-mode'.")

(custom-autoload 'telega-appindicator-mode "telega-modes" nil)

(autoload 'telega-appindicator-mode "telega-modes" "\
Toggle display of the unread chats/mentions in the system tray.

This is a minor mode.  If called interactively, toggle the
`Telega-Appindicator mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='telega-appindicator-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(defvar telega-autoplay-mode nil "\
Non-nil if Telega-Autoplay mode is enabled.
See the `telega-autoplay-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `telega-autoplay-mode'.")

(custom-autoload 'telega-autoplay-mode "telega-modes" nil)

(autoload 'telega-autoplay-mode "telega-modes" "\
Automatically play animation messages.

This is a minor mode.  If called interactively, toggle the
`Telega-Autoplay mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='telega-autoplay-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'telega-squash-message-mode "telega-modes" "\
Toggle message squashing minor mode.

This is a minor mode.  If called interactively, toggle the
`Telega-Squash-Message mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `telega-squash-message-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(defvar global-telega-squash-message-mode nil "\
Non-nil if Global Telega-Squash-Message mode is enabled.
See the `global-telega-squash-message-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-telega-squash-message-mode'.")

(custom-autoload 'global-telega-squash-message-mode "telega-modes" nil)

(autoload 'global-telega-squash-message-mode "telega-modes" "\
Global mode to squashing messages.

This is a minor mode.  If called interactively, toggle the
`Global Telega-Squash-Message mode' mode.  If the prefix argument
is positive, enable the mode, and if it is zero or negative,
disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='global-telega-squash-message-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'telega-edit-file-mode "telega-modes" "\
Minor mode to edit files from Telegram messages.
Can be enabled only for content from editable messages.

This is a minor mode.  If called interactively, toggle the
`Telega-Edit-File mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `telega-edit-file-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "telega-modes" '("telega-"))

;;;***

;;;### (autoloads nil "telega-msg" "telega-msg.el" (0 0 0 0))
;;; Generated autoloads from telega-msg.el

(register-definition-prefixes "telega-msg" '("telega-"))

;;;***

;;;### (autoloads nil "telega-notifications" "telega-notifications.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from telega-notifications.el

(defvar telega-notifications-mode nil "\
Non-nil if Telega-Notifications mode is enabled.
See the `telega-notifications-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `telega-notifications-mode'.")

(custom-autoload 'telega-notifications-mode "telega-notifications" nil)

(autoload 'telega-notifications-mode "telega-notifications" "\
Telega D-Bus notifications.

This is a minor mode.  If called interactively, toggle the
`Telega-Notifications mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='telega-notifications-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "telega-notifications" '("telega-"))

;;;***

;;;### (autoloads nil "telega-obsolete" "telega-obsolete.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from telega-obsolete.el

(register-definition-prefixes "telega-obsolete" '("telega-obsolete--"))

;;;***

;;;### (autoloads nil "telega-root" "telega-root.el" (0 0 0 0))
;;; Generated autoloads from telega-root.el

(register-definition-prefixes "telega-root" '("telega-" "with-telega-root-view-ewoc"))

;;;***

;;;### (autoloads nil "telega-server" "telega-server.el" (0 0 0 0))
;;; Generated autoloads from telega-server.el

(autoload 'telega-server-build "telega-server" "\
Build and install `telega-server' binary.
If BUILD-FLAGS is specified, then rebuild server without any
queries using this flags for building, could be empty string.
Otherwise query user about building flags.

\(fn &optional BUILD-FLAGS)" t nil)

(register-definition-prefixes "telega-server" '("telega-" "with-telega-deferred-events"))

;;;***

;;;### (autoloads nil "telega-sort" "telega-sort.el" (0 0 0 0))
;;; Generated autoloads from telega-sort.el

(register-definition-prefixes "telega-sort" '("chat" "define-telega-sorter" "join-date" "last-seen" "member-count" "nearby-distance" "online-members" "order" "telega-" "title" "unread-count"))

;;;***

;;;### (autoloads nil "telega-sticker" "telega-sticker.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from telega-sticker.el

(register-definition-prefixes "telega-sticker" '("telega-"))

;;;***

;;;### (autoloads nil "telega-tdlib" "telega-tdlib.el" (0 0 0 0))
;;; Generated autoloads from telega-tdlib.el

(register-definition-prefixes "telega-tdlib" '("telega-" "with-telega-server-reply"))

;;;***

;;;### (autoloads nil "telega-tdlib-events" "telega-tdlib-events.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from telega-tdlib-events.el

(register-definition-prefixes "telega-tdlib-events" '("telega-" "tracking-buffers" "with-telega--msg-update-event"))

;;;***

;;;### (autoloads nil "telega-tme" "telega-tme.el" (0 0 0 0))
;;; Generated autoloads from telega-tme.el

(register-definition-prefixes "telega-tme" '("telega-tme-"))

;;;***

;;;### (autoloads nil "telega-user" "telega-user.el" (0 0 0 0))
;;; Generated autoloads from telega-user.el

(register-definition-prefixes "telega-user" '("telega-"))

;;;***

;;;### (autoloads nil "telega-util" "telega-util.el" (0 0 0 0))
;;; Generated autoloads from telega-util.el

(register-definition-prefixes "telega-util" '("telega-" "with-telega-symbol-animate"))

;;;***

;;;### (autoloads nil "telega-voip" "telega-voip.el" (0 0 0 0))
;;; Generated autoloads from telega-voip.el

(register-definition-prefixes "telega-voip" '("telega-"))

;;;***

;;;### (autoloads nil "telega-vvnote" "telega-vvnote.el" (0 0 0 0))
;;; Generated autoloads from telega-vvnote.el

(register-definition-prefixes "telega-vvnote" '("telega-"))

;;;***

;;;### (autoloads nil "telega-webpage" "telega-webpage.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from telega-webpage.el

(register-definition-prefixes "telega-webpage" '("telega-"))

;;;***

;;;### (autoloads nil nil ("telega-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; telega-autoloads.el ends here
