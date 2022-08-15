;;; yasnippet-lean.el --- Collection of snippets for the Lean prover

;; adapted from https://github.com/AndreaCrotti/yasnippet-snippets/blob/master/yasnippet-snippets.el

;; Copyright (C) 2017 Andrea Crotti

;; Authors: Andrea Crotti <andrea.crotti.0@gmail.com>,
;;          Simon Hudon <simon.hudon@gmail.com>
;; Version: 0.1
;; Package-Requires: ((yasnippet "0.8.0"))
;; Keywords: convenience, snippets, leanprover
;; Maintainer: Simon Hudon <simon.hudon@gmail.com>
;; URL: https://github.com/leanprover-community/yasnippet-lean

;;; Commentary:

;; Snippet collection for the Lean theorem prover.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'yasnippet)
(require 'lean-mode)
(defconst yasnippet-lean-dir
  (expand-file-name
   "snippets"
   (file-name-directory
    ;; Copied from ‘f-this-file’ from f.el.
    (cond
     (load-in-progress load-file-name)
     ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
      byte-compile-current-file)
     (:else (buffer-file-name))))))

;;;###autoload
(defun yasnippet-lean-initialize ()
  "Load the `yasnippet-lean' snippets directory."
  (add-to-list 'yas-snippet-dirs 'yasnippet-lean-dir t)
  (yas-load-directory yasnippet-lean-dir t))

(defgroup yasnippet-lean nil
  "Options for yasnippet setups.
This is useful for customizing options declared in
“.yas-setup.el” files.  For example, you could declare a
customizable variable used for a snippet expansion.
See Info node `(elisp)Customization Types'."
  :group 'yasnippet)

;;;###autoload
(eval-after-load 'yasnippet
   '(yasnippet-lean-initialize))

(provide 'yasnippet-lean)

;;; yasnippet-lean.el ends here
