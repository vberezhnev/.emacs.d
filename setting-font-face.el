(set-face-attribute 'default nil
  :font "Source Code Pro"
  :height 110
  :weight 'medium)
;(set-face-attribute 'variable-pitch nil
;  :font "Ubuntu Nerd Font"
;  :height 120
;  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "Source Code Pro"
  :height 110
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
(add-to-list 'default-frame-alist '(font . "Source Code Pro-11"))
;; changes certain keywords to symbols, such as lamda!
(setq global-prettify-symbols-mode t)
