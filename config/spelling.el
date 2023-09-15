(use-package ispell
  :bind ("<f8>" . ispell-word) ; easy spell check
  :custom
  (ispell-program-name "hunspell") ; require Hunspell
  (ispell-dictionary "en_US,en_GB,bn_BD")
  (ispell-personal-dictionary "~/.emacs.d/.hunspell_personal")
  :config
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (setenv "LANG" "en_US.UTF-8")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic ispell-dictionary)
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0)))

(use-package flyspell
  :bind (:map flyspell-mode-map
              ("C-;"        . nil)
              ("C-,"        . nil)
              ("C-."        . nil)
              ("M-TAB"      . nil)
              ("C-x M-$"    . flyspell-buffer)
              ("C-<f7>"     . flyspell-auto-correct-word)
              ("C-<f12>"    . flyspell-auto-correct-previous-word))
  :init (progn (dolist (hook '(org-mode-hook text-mode-hook message-mode-hook))
                 (add-hook hook 'turn-on-flyspell))
               (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :delight " ⓢ")
