;;; dired.el --- Dired configuration for directory navigation -*- lexical-binding: t; -*-

;; Dired: Load for directory navigation
(use-package dired
  :ensure nil ;; Built-in package, no straight.el needed
  :straight (:type built-in)
  :commands (dired dired-jump dired-omit-mode)
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . dired-omit-mode))
  ;; :bind (:map dired-mode-map
  ;;        ("." . dired-omit-mode))
  :init
  (setq dired-mouse-drag-files t
        dired-listing-switches "-l -A -h -v --group-directories-first"
        dired-omit-files (rx (or (seq bol (? ".") "#"))))
  ;; :config
  ;; (general-unbind :states 'normal :keymaps 'dired-mode-map
  ;;   "SPC"
  ;;   "S-SPC")
  )

;; All-the-icons-dired: Load for dired with icons
(use-package all-the-icons-dired
  :straight t
  :after dired
  :commands (all-the-icons-dired-mode)
  :hook (dired-mode . all-the-icons-dired-mode))

;; Dired open extensions (unchanged, lightweight global setting)
(setq dired-open-extensions
      '(("gif" . "eog")
        ("jpg" . "eog")
        ("png" . "eog")
        ("mkv" . "vlc --one-instance")
        ("mp4" . "vlc --one-instance")
        ("ogv" . "vlc --one-instance")
        ("ogg" . "vlc --one-instance")
        ("avi" . "vlc --one-instance")
        ("odt" . "libreoffice")
        ("docx" . "libreoffice")
        ("doc" . "libreoffice")))

;; Dired-rainbow: Load for file type highlighting
(use-package dired-rainbow
  :straight t
  :after dired
  :commands (dired-rainbow-define)
  :config
  (dired-rainbow-define img nil ("gif" "jpg" "png"))
  (dired-rainbow-define video nil ("mp4" "mkv" "ogv" "ogg" "avi"))
  (dired-rainbow-define office nil ("odt" "doc" "docx"))
  (dired-rainbow-define pdf nil ("pdf"))
  (set-face-attribute 'dired-rainbow-img-face nil :foreground my/green)
  (set-face-attribute 'dired-rainbow-office-face nil :foreground my/yellow)
  (set-face-attribute 'dired-rainbow-video-face nil :foreground my/orange)
  (set-face-attribute 'dired-rainbow-pdf-face nil :foreground my/purple))

(use-package openwith
  :straight t
  :config
  (setq openwith-associations
        (cond
         ((string-equal system-type "darwin")
          '(("\\.\\(dmg\\|doc\\|docs\\|xls\\|xlsx\\)$"
             "open" (file))
            ("\\.\\(mp4\\|mp3\\|webm\\|avi\\|flv\\|mov\\)$"
             "open" ("-a" "VLC" file))))
         ((string-equal system-type "gnu/linux")
          '(("\\.\\(mp4\\|mp3\\|webm\\|avi\\|flv\\|mov\\|ogg\\)$"
             "xdg-open" (file))))))
  (openwith-mode +1))

(use-package dired-sidebar
  :straight t
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(provide 'dired)
;;; dired.el ends here
