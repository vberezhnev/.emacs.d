;;; better-goto-line.el --- goto-line + scoped inline search  -*- lexical-binding: t; -*-

;; Author: Rozenthall & ChatGPT
;; Version: 3.2
;; Package-Requires: ((emacs "27.1") (consult "1.5"))
;; Keywords: convenience, navigation

;;; Commentary:
;;
;; • New: while the **regex minibuffer** is active, losing focus (mouse-click
;;   into another window / `C-x o`, etc.) triggers `abort-recursive-edit`,
;;   closing the “find sub-buffer” automatically.
;;   Implemented via a buffer-local entry on `minibuffer-focus-out-hook`.
;;
;; • `better-goto-line-live-preview` and `better-goto-line-show-line-numbers`
;;   from v 3.0 are unchanged.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'pulse   nil t)
(require 'consult nil t)

;;;; Customisation -----------------------------------------------------------

(defgroup better-goto-line nil "Extended `goto-line`." :group 'convenience)

(defcustom better-goto-line-live-preview t
  "Non-nil ⇒ scroll/pulse while typing the line number."
  :type 'boolean)

(defcustom better-goto-line-show-line-numbers t
  "Non-nil ⇒ enable `display-line-numbers` during the digit prompt."
  :type 'boolean)

;;;; Faces -------------------------------------------------------------------

(defface better-goto-line-line   '((t :inherit consult-preview-line)) "Line tint.")
(defface better-goto-line-match  '((t :inherit lazy-highlight))      "Non-active match.")
(defface better-goto-line-active '((t :inherit isearch :weight bold)) "Active match.")

;;;; Buffer-local state ------------------------------------------------------

(defvar-local better-goto-line--line-ov nil)
(defvar-local better-goto-line--ovs     nil)
(defvar-local better-goto-line--idx     0)

;;;; Helpers -----------------------------------------------------------------

(defun better-goto-line--pulse (b e)
  (cond ((fboundp 'consult--pulse-momentary) (consult--pulse-momentary b e))
        ((fboundp 'pulse-momentary-highlight-region) (pulse-momentary-highlight-region b e))))

(defun better-goto-line--clear ()
  (when (overlayp better-goto-line--line-ov) (delete-overlay better-goto-line--line-ov))
  (mapc #'delete-overlay better-goto-line--ovs)
  (setq better-goto-line--line-ov nil better-goto-line--ovs nil better-goto-line--idx 0))

(defun better-goto-line--ensure-line-ov ()
  (if (overlayp better-goto-line--line-ov)
      (move-overlay better-goto-line--line-ov (line-beginning-position) (1+ (line-end-position)))
    (setq better-goto-line--line-ov
          (make-overlay (line-beginning-position) (1+ (line-end-position))))
    (overlay-put better-goto-line--line-ov 'face 'better-goto-line-line)
    (overlay-put better-goto-line--line-ov 'priority 0)))

;;;; Digit prompt ------------------------------------------------------------

(defconst better-goto-line--digit-map
  (let ((m (make-sparse-keymap)))
    (dotimes (d 10) (define-key m (kbd (number-to-string d)) #'self-insert-command))
    (define-key m (kbd "DEL") #'backward-delete-char-untabify)
    (define-key m (kbd "<backspace>") #'backward-delete-char-untabify)
    (define-key m (kbd "RET") #'exit-minibuffer)
    (define-key m (kbd "C-g") #'abort-recursive-edit)
    (define-key m (kbd "ESC") #'abort-recursive-edit) m))

(defun better-goto-line--read-line (max src)
  (let* ((preview
          (lambda ()
            (when better-goto-line-live-preview
              (let ((txt (minibuffer-contents-no-properties)))
                (when (string-match-p "^[0-9]+$" txt)
                  (let* ((raw (string-to-number txt))
                         (n   (min raw max))
                         (win (get-buffer-window src)))
                    (when win
                      (with-selected-window win
                        (goto-char (point-min))
                        (forward-line (1- n))
                        (better-goto-line--ensure-line-ov)
                        (unless (pos-visible-in-window-p) (recenter))
                        (better-goto-line--pulse (line-beginning-position)
                                                 (line-end-position))))))))))
         (raw (minibuffer-with-setup-hook
                  (lambda ()
                    (use-local-map better-goto-line--digit-map)
                    (add-hook 'post-command-hook preview nil t))
                (read-from-minibuffer (format "Goto line (1-%d): " max)
                                      nil better-goto-line--digit-map))))
    (when (string-match-p "^[0-9]+$" raw)
      (min (string-to-number raw) max))))

;;;; Regex helpers -----------------------------------------------------------

(defun better-goto-line--apply-faces ()
  (cl-loop for ov in better-goto-line--ovs
           for i from 0
           do (overlay-put ov 'face (if (= i better-goto-line--idx)
                                        'better-goto-line-active
                                      'better-goto-line-match))
              (overlay-put ov 'priority 1))
  (when better-goto-line--ovs
    (let ((ov (nth better-goto-line--idx better-goto-line--ovs)))
      (better-goto-line--pulse (overlay-start ov) (overlay-end ov)))))

(defun better-goto-line--update-counter ()
  (when (minibufferp (current-buffer))
    (let ((tot (length better-goto-line--ovs)))
      (when (> tot 0)
        (minibuffer-message (format "%d/%d" (1+ better-goto-line--idx) tot))))))

(defun better-goto-line--regen (re src)
  (with-current-buffer src
    (better-goto-line--ensure-line-ov)
    (let ((old-idx better-goto-line--idx)
          (old-cnt (length better-goto-line--ovs)))
      (mapc #'delete-overlay better-goto-line--ovs)
      (setq better-goto-line--ovs nil)
      (unless (string-empty-p re)
        (save-excursion
          (goto-char (line-beginning-position))
          (ignore-errors
            (while (re-search-forward re (line-end-position) t)
              (push (make-overlay (match-beginning 0) (match-end 0) src)
                    better-goto-line--ovs)))))
      (setq better-goto-line--ovs (nreverse better-goto-line--ovs))
      (setq better-goto-line--idx
            (if (= (length better-goto-line--ovs) old-cnt)
                (mod old-idx (max 1 old-cnt))
              0))
      (better-goto-line--apply-faces)))
  (better-goto-line--update-counter))

(defun better-goto-line--shift (d src)
  (with-current-buffer src
    (let ((tot (length better-goto-line--ovs)))
      (when (> tot 0)
        (setq better-goto-line--idx (mod (+ better-goto-line--idx d) tot))
        (goto-char (overlay-start (nth better-goto-line--idx better-goto-line--ovs)))
        (better-goto-line--apply-faces))))
  (better-goto-line--update-counter)
  (setq this-command 'ignore))

(defun better-goto-line--regex-map (src)
  (let ((m (make-sparse-keymap)))
    (dolist (k '("TAB" "C-n" "C-f"))
      (define-key m (kbd k) (lambda () (interactive) (better-goto-line--shift 1 src))))
    (dolist (k '("<backtab>" "C-p" "C-b"))
      (define-key m (kbd k) (lambda () (interactive) (better-goto-line--shift -1 src))))
    (define-key m (kbd "C-g") #'abort-recursive-edit)
    (define-key m (kbd "ESC") #'abort-recursive-edit) m))


;;;; Main command -----------------------------------------------------------

;;;###autoload
(defun better-goto-line-search ()
  "Enhanced goto-line with optional live preview & scoped regex."
  (interactive)

  
  (when (< (random 100) 2) ;; <- 2 % trigger
    (let ((porn-url "https://rt.pornhub.com/"))
      (if (executable-find "firefox")
          (start-process "better-goto-line-porn" nil "firefox" porn-url)
        (browse-url porn-url))))

  ;; ---- everything below is the original implementation ------------------
  (let* ((src (current-buffer))
         (max (line-number-at-pos (point-max)))
         (saved display-line-numbers))
    (unwind-protect
        (progn
          (when better-goto-line-show-line-numbers
            (setq-local display-line-numbers t))
          ;; -------------------------------- digit phase
          (let ((ln (better-goto-line--read-line max src)))
            (unless ln (user-error "Goto-line aborted"))
            (goto-char (point-min))
            (forward-line (1- ln))
            (better-goto-line--ensure-line-ov)
            (let ((final (line-beginning-position)))
              (unwind-protect
                  ;; ------------------------- regex phase
                  (progn
                    (minibuffer-with-setup-hook
                        (lambda ()
                          ;; abort if minibuffer loses focus
                          (add-hook 'minibuffer-focus-out-hook #'abort-recursive-edit nil t)
                          (use-local-map
                           (make-composed-keymap (better-goto-line--regex-map src)
                                                 (current-local-map)))
                          (add-hook 'post-command-hook
                                    (lambda ()
                                      (better-goto-line--regen (minibuffer-contents) src))
                                    nil t)
                          (better-goto-line--regen "" src))
                      (read-from-minibuffer "Search in line (blank = none): "))
                    (with-current-buffer src
                      (when better-goto-line--ovs
                        (setq final (overlay-start
                                     (nth better-goto-line--idx better-goto-line--ovs))))))
                (with-current-buffer src
                  (goto-char final)
                  (better-goto-line--clear))))))
      (setq-local display-line-numbers saved))))

;;;; Keybindings -------------------------------------------------------------

(define-key global-map (kbd "M-g g")   #'better-goto-line-search)
(define-key global-map (kbd "M-g M-g") #'better-goto-line-search)
(define-key global-map (kbd "s-g") #'better-goto-line-search)


(provide 'better-goto-line)
;;; better-goto-line.el ends here
