;;; chep-video-el  ---  allows you to play videos in emacs
;;
;; Filname: chep-video-el
;; Description: allows you to play videos in emacs
;; Author: Cédric Chépied <cedric.chepied@gmail.com>
;; Maintainer: Cédric Chépied
;; Copyright (C) 2013, Cédric Chépied
;; Last updated: Th Jul 25th
;;     By Cédric Chépied
;;     Update 1
;; Keywords: video mplayer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;The way we play video is ugly but works: mplayer is called with -vo png
;;so it plays sound and extracts video frames in /tmp/chep-video.d as png
;;files. A timer is set in emacs to periodically display and delete images.
;;
;; Keys and interactive functions:
;; chep-video-play:                           start playing a video
;; chep-video-stop:                           stop current playing video
;; chep-video-playPause (space):              toggle play pause state
;; chep-video-forward (right):                go 10 sec forward
;; chep-video-backward (left):                go 10 sec backward
;; chep-video-forward-long (up):              go 1 min forward
;; chep-video-backward-long (down):           go 1 min backward
;; chep-video-forward-long-long (page up):    go 10 min forward
;; chep-video-backward-long-long (page down): go 10 min backward
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; Copyright Cédric Chépied 2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar chep-video-buffer nil)
(defvar chep-video-frame 1)
(defvar chep-video-timer nil)
(defvar chep-video-process nil)
(defvar chep-video-process-buffer nil)

(defconst chep-video-images-path "/tmp/chep-video.d")

(defvar chep-video-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map (kbd "SPC") 'chep-video-playPause)
      (define-key map (kbd "<right>") 'chep-video-forward)
      (define-key map (kbd "<left>") 'chep-video-backward)
      (define-key map (kbd "<up>") 'chep-video-forward-long)
      (define-key map (kbd "<down>") 'chep-video-backward-long)
      (define-key map (kbd "<next>") 'chep-video-backward-long-long)
      (define-key map (kbd "<prior>") 'chep-video-forward-long-long)
      (define-key map "q" 'chep-video-stop-and-bury)))
  "Local keymap for chep-video.")

(defun chep-video-mode ()
  (kill-all-local-variables)
  (setq major-mode 'chep-video-mode
        mode-name "VIDEO")
  (use-local-map chep-video-mode-map))

(defun chep-video-play (fichier)
  (interactive (list (read-file-name "Vidéo:")))

  ;;stop if already running
  (when chep-video-process
    (chep-video-stop))

  ;;creating buffers
  (setq chep-video-process-buffer (get-buffer-create "*video-process*"))
  (setq chep-video-buffer (get-buffer-create "*video*"))
  (set-window-buffer (selected-window) chep-video-buffer)
  (set-buffer chep-video-buffer)
  (chep-video-mode)

  ;;temp directory:
  (when (file-exists-p chep-video-images-path)
    (if (file-directory-p chep-video-images-path)
        (delete-directory chep-video-images-path t)
      (delete-file chep-video-images-path)))
  (make-directory chep-video-images-path t)


  ;;creating mplayer process and refresh timer
  (setq chep-video-process (start-process "video-process"
                                          chep-video-process-buffer
                                          "mplayer" "-slave"
                                          "-vo" (concat "png:outdir=" chep-video-images-path)
                                          "-zoom" "-xy" (int-to-string (window-pixel-width))
                                          "-osdlevel" "1" "-quiet"
                                          fichier)
        chep-video-frame 1
        chep-video-timer (run-at-time 0 0.01
                                      'chep-video-render-img)))

(defun chep-video-render-img ()
  (if (string= (process-status chep-video-process) "run")
      (let ((fichier (format "%s/%08d.png"
                             chep-video-images-path
                             chep-video-frame))
            (precedent (format "%s/%08d.png"
                               chep-video-images-path
                               (1- chep-video-frame)))
            (suivante (format "%s/%08d.png"
                              chep-video-images-path
                              (1+ chep-video-frame))))
        (when (and (file-exists-p fichier)
                   (file-exists-p suivante))
          (let ((buffer (current-buffer)))
            (set-buffer chep-video-buffer)
            (when (not (= chep-video-frame 1))
              (delete-file precedent))
            (erase-buffer)
            (insert-image (create-image fichier))
            (setq chep-video-frame (1+ chep-video-frame))
            (set-buffer buffer))))
    (chep-video-stop)))

(defun chep-video-stop ()
  (interactive)
  (ignore-errors
    (cancel-timer chep-video-timer)
    (interrupt-process chep-video-process)
    (stop-process chep-video-process)
    (setq chep-video-timer nil
          chep-video-process nil)
    (delete-file (format "%s/%08d.png"
                         chep-video-images-path
                         (1- chep-video-frame)))))

(defun chep-video-stop-and-bury ()
  (interactive)
  (chep-video-stop)
  (bury-buffer))

(defun chep-video-playPause ()
  (interactive)
  (when chep-video-process
    (process-send-string chep-video-process "pause\n")))

(defun chep-video-forward ()
  (interactive)
  (when chep-video-process
    (process-send-string chep-video-process "seek 10\n")))

(defun chep-video-backward ()
  (interactive)
  (when chep-video-process
    (process-send-string chep-video-process "seek -10\n")))

(defun chep-video-forward-long ()
  (interactive)
  (when chep-video-process
    (process-send-string chep-video-process "seek 60\n")))

(defun chep-video-backward-long ()
  (interactive)
  (when chep-video-process
    (process-send-string chep-video-process "seek -60\n")))


(defun chep-video-forward-long-long ()
  (interactive)
  (when chep-video-process
    (process-send-string chep-video-process "seek 600\n")))

(defun chep-video-backward-long-long ()
  (interactive)
  (when chep-video-process
    (process-send-string chep-video-process "seek -600\n")))

(provide 'chep-video)
