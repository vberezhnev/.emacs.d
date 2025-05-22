;; Directories first
;; (setq dired-use-ls-dired  nil)
;; (setq insert-directory-program "/usr/bin/ls")

(use-package dired
  :hook ((dired-mode . dired-hide-details-mode)
				 (dired-mode . dired-omit-mode))
	:bind (:map dired-mode-map
							( "."     . dired-omit-mode))
  ;; :custom (dired-omit-files (rx (seq bol ".")))
	:config
	(setq dired-listing-switches "-l -A -h -v --group-directories-first")
	(setq dired-omit-files
				(rx (or (seq bol (? ".") "#")))))

;; (setq dired-listing-switches "-al --group-directories-first")

(use-package all-the-icons-dired
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(setq dired-open-extensions
      '(("gif" . "eog")
        ("jpg" . "eog")
        ("png" . "eog")
        ("mkv" . "vlc --one-instance")
        ("mp4" . "vlc --one-instance")
        ("ogv" . "vlc --one-instance")
        ("avi" . "vlc --one-instance")
        ("odt"  . "libreoffice")
        ("docx" . "libreoffice")
        ("docx" . "libreoffice")))

(use-package dired-rainbow
  :ensure t)

(dired-rainbow-define img nil ("gif" "jpg" "png"))
(dired-rainbow-define video nil ("mp4" "mkv" "ogv" "avi"))
(dired-rainbow-define office nil ("odt" "doc" "docx"))
(dired-rainbow-define pdf nil ("pdf"))

(set-face-attribute 'dired-rainbow-img-face nil :foreground my/green)
(set-face-attribute 'dired-rainbow-office-face nil :foreground my/yellow)
(set-face-attribute 'dired-rainbow-video-face nil :foreground my/orange)
(set-face-attribute 'dired-rainbow-pdf-face nil :foreground my/purple)

;; (use-package dired
;;   :hook (dired-mode . hl-line-mode))

(general-unbind 'normal dired-mode-map
  "SPC"
  "S-SPC")

(provide 'dired)
