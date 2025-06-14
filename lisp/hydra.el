;; (use-package hydra
;;   :ensure t
;;   :custom
;;   (hydra-default-hint nil))

;; (general-nmap "SPC" 'hydra-main/body)
;; (general-mmap "SPC" 'hydra-main/body)

;; ;; (use-package hydra-posframe
;; ;;   :load-path "/home/jkadlcik/git/hydra-posframe"
;; ;;   :hook
;; ;;   (after-init . hydra-posframe-enable))

;; (use-package pretty-hydra
;;   :ensure t)

;; (pretty-hydra-define hydra-main
;;   (:color blue :quit-key ("q" "SPC") :title "Hydra" :idle 1.0)
;;   ("Misc"
;;    (("m" major-mode-hydra "major mode")
;;     ("a" hydra-applications/body "applications")
;;     ("o" browse-url-at-point "open URL")
;;     ("z" hydra-presentations/body "presentations"))

;;    "Workspaces"
;;    (("e" eyebrowse-switch-to-window-config "switch workspace")
;;     ("E" hydra-eyebrowse/body "eyebrowse"))

;;    "Projects"
;;    (("p" projectile-switch-project "switch project")
;;     ("P" hydra-project/body "projectile"))

;;    "Window management"
;;    (("w" ace-window "ace-window")
;;     ("W" hydra-window/body "window management"))

;;    "File Management"
;;    (("d" dired-jump "open dired in the current directory")
;;     ("D" hydra-dired/body "dired"))

;;    "Bookmarks"
;;    (("B" hydra-bookmarks/body "bookmarks"))

;;    "Git"
;;    (("g" magit-status "magit")
;;     ("G" hydra-magit/body "more magit options"))

;;    "LSP"
;;    (("l" hydra-lsp/body "LSP"))

;;    "Time"
;;    (("t" org-pomodoro "Pomodoro"))

;;    "Emacs"
;;    (("r" (load-file user-init-file) "reload configuration")
;;     ("h" hydra-help/body "help")
;;     ("c" hydra-configuration/body "configure"))))

;; (pretty-hydra-define hydra-help
;;   (:color blue :quit-key ("q" "SPC") :title "Help" :idle 1.0)
;;   ("Help"
;;    (("h" #'help "help"))

;;   "Describe"
;;    (("m" #'describe-mode "mode")
;;     ("F" #'describe-face "face")
;;     ("k" #'describe-key "key")
;;     ("v" #'describe-variable "variable")
;;     ("f" #'describe-function "function")
;;     ("c" #'describe-command "command"))))

;; (pretty-hydra-define hydra-bookmarks
;;   (:color blue :quit-key ("q" "SPC") :title "Bookmarks" :idle 1.0)
;;   ("Bookmarks"
;;    (("c" bookmark-set "Create a bookmark")
;;     ("l" helm-bookmarks "List and jump bookmarks")
;;     ("s" bookmark-save "Save bookmarks to disk"))))

;; (pretty-hydra-define hydra-magit
;;   (:color blue :quit-key ("q" "SPC") :title "Magit" :idle 1.0)
;;   ("Open in"
;;    (("g" magit-status "magit")
;;     ("b" magit-branch-checkout))))

;; (pretty-hydra-define hydra-lsp
;;   (:color blue :quit-key ("q" "SPC") :title "Magit" :idle 1.0)
;;   ("Server"
;;    (("w" lsp-describe-session "Describe session"))

;;    "Navigation"
;;    (("d" lsp-find-definition "Jump to definition")
;;     ("r" lsp-find-references "Find references"))

;;    "Documentation"
;;    (("D" lsp-describe-thing-at-point "Show documentation"))

;;    "Refactoring"
;;    (("R" lsp-rename "Rename this thing"))

;;    "Spellcheck"
;;    (("s" lsp-grammarly-check-grammar "Spellcheck using Grammarly.com")
;;     ("S" lsp-grammarly-stop "Disable spellcheck"))))

;; (pretty-hydra-define hydra-linter
;;   (:color blue :quit-key ("q" "SPC") :title "Linter" :idle 1.0)
;;   ("Flycheck"
;;    (("v" flycheck-verify-setup "verify setup")
;;     ("t" flycheck-mode "toggle on/off"))

;;    "Error"
;;    (("<" flycheck-previous-error "previous")
;;     (">" flycheck-next-error "next")
;;     ("l" flycheck-list-errors "list")
;;     ("o" nil "open in browser")
;;     ("i" pylint-disable-current-warning "ignore"))))

;; (pretty-hydra-define hydra-applications
;;   (:color blue :quit-key ("q" "SPC") :title "Applications" :idle 1.0)
;;   ("Launch"
;;    (("a" (org-agenda nil "f") "Org agenda")
;;     ("r" elfeed "RSS (elfeed)")
;;     ("t" frostyx/multi-vterm-named "Terminal (vterm)")
;;     ("w" eww "web (eww)")
;;     ("e" mu4e "email (mu4e)")
;;     ("s" hydra-spotify/body "Spotify"))))
