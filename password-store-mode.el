;;; password-store-mode.el --- Major mode for password-store.el

;; Copyright (C) 2015  Nicolas Petton

;; Author: Nicolas Petton <petton.nicolas@gmail.com>
;; Keywords: password-store, password, keychain

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for password-store.el

;; TODO:
;; - get a tree structure of entries, not a flat list
;; - add navigation to the next/previous folder with M-n and M-p

;;; Code:
(require 'password-store)

(defvar password-store-mode-buffer-name "*Password-Store*"
  "Name of the password-store-mode buffer.")

(defvar password-store-mode-hook nil
  "Mode hook for `password-store-mode'.")

(defvar password-store-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'password-store-mode-next-entry)
    (define-key map (kbd "p") #'password-store-mode-prev-entry)
    (define-key map (kbd "k") #'password-store-mode-kill)
    (define-key map (kbd "s") #'isearch-forward)
    (define-key map (kbd "r") #'isearch-backward)
    (define-key map (kbd "?") #'describe-mode)
    (define-key map (kbd "g") #'password-store-mode-update-buffer)
    (define-key map (kbd "i") #'password-store-mode-insert)
    (define-key map (kbd "w") #'password-store-mode-copy)
    (define-key map (kbd "v") #'password-store-mode-view)
    (define-key map (kbd "RET") #'password-store-mode-view)
    (define-key map (kbd "q") #'password-store-mode-quit)
    map)
  "Keymap for `password-store-mode'.")

(defface password-store-mode-header-face '((t . (:inherit font-lock-keyword-face)))
  "Face for displaying the header of the password-store-mode buffer.")

(defface password-store-mode-directory-face '((t . ()))
  "Face for displaying password-store-mode field names.")

(defface password-store-mode-entry-face '((t . (:inherit
                                                font-lock-function-name-face
                                                :weight
                                                bold)))
  "Face for displaying password-store entry names.")

(defface password-store-mode-password-face '((t . (:inherit widget-field)))
  "Face for displaying password-store entrys names.")

(defun password-store-mode ()
  "Major mode for editing password-stores.

\\{password-store-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'password-store-mode
        mode-name 'Password-Store)
  (read-only-mode)
  (use-local-map password-store-mode-map)
  (run-hooks 'password-store-mode-hook))

(defun password-store-mode-setup-buffer ()
  "Setup the password-store buffer."
  (password-store-mode)
  (password-store-mode-update-buffer))

(defun password-store ()
  "Open the password-store buffer."
  (interactive)
  (if (get-buffer password-store-mode-buffer-name)
      (switch-to-buffer password-store-mode-buffer-name)
    (progn
      (let ((buf (get-buffer-create password-store-mode-buffer-name)))
        (pop-to-buffer buf)
        (password-store-mode-setup-buffer)))))

(defun password-store-mode-quit ()
  "Kill the buffer quitting the window and forget the password-store-mode."
  (interactive)
  (quit-window t))

(defun password-store-mode-next-entry ()
  "Move point to the next entry found."
  (interactive)
  (password-store-mode--goto-next #'password-store-mode-entry-at-point))

(defun password-store-mode-prev-entry ()
  "Move point to the previous entry."
  (interactive)
  (password-store-mode--goto-prev #'password-store-mode-entry-at-point))

(defun password-store-mode-kill ()
  "Remove the entry at point."
  (interactive)
  (with-closest-entry entry
    (when (yes-or-no-p (format "Do you want remove the entry %s? " entry))
      (password-store-remove entry)
      (password-store-mode-update-buffer))))

(defun password-store-mode-update-buffer ()
  "Update the current buffer contents."
  (interactive)
  (save-point
    (with-writable-buffer
      (delete-region (point-min) (point-max))
      (password-store-mode-display-data))))

(defun password-store-mode-insert (&optional arg)
  "Insert an entry to the password-store.
When called with a prefix argument ARG, use a generated password
instead of reading the password from user input."
  (interactive "P")
  (if arg
      (call-interactively #'password-store-generate)
    (call-interactively #'password-store-insert))
  (password-store-mode-update-buffer))

(defun password-store-mode-view ()
  "Visit the entry at point."
  (interactive)
  (with-closest-entry entry
    (password-store-edit entry)))

(defun password-store-mode-copy ()
  "Visit the entry at point."
  (interactive)
  (with-closest-entry entry
    (password-store-copy entry)))

(defun password-store-mode-display-data ()
  "Display the password-store data into the current buffer."
  (let ((entries (sort (password-store-mode--get-entries)
                       #'string-lessp)))
    (password-store-mode-display-header)
    (dolist (entry entries)
      (password-store-mode-display-entry entry))))

(defun password-store-mode-display-header ()
  "Display the header intothe current buffer."
  (insert "Password-store contents:")
  (put-text-property (point-at-bol) (point) 'face 'password-store-mode-header-face)
  (insert " ")
  (newline)
  (newline))

(defun password-store-mode-display-entry (entry)
  "Display the entry ENTRY and the associated data into the current buffer."
  (insert entry)
  (add-text-properties (point-at-bol) (point)
                       `(face password-store-mode-entry-face password-store-mode-entry ,entry))
  (newline))

(defun password-store-mode-entry-at-point ()
  "Return the `password-store-mode-entry' property at point."
  (get-text-property (point) 'password-store-mode-entry))

(defun password-store-mode-closest-entry ()
  "Return the closest entry in the current buffer, looking backward."
  (save-excursion
    (unless (bobp)
      (or (password-store-mode-entry-at-point)
          (progn
            (previous-line)
            (password-store-mode-closest-entry))))))

(defun password-store-mode--goto-next (pred)
  "Move point to the next match of PRED."
  (next-line)
  (while (not (or (eobp) (funcall pred)))
    (next-line)))

(defun password-store-mode--goto-prev (pred)
  "Move point to the previous match of PRED."
  (previous-line)
  (while (not (or (bobp) (funcall pred)))
    (previous-line)))

(defmacro with-writable-buffer (&rest body)
  "Evaluate BODY with the current buffer not in `read-only-mode'."
  (declare (indent 0) (debug t))
  (let ((read-only (make-symbol "ro")))
    `(let ((,read-only buffer-read-only))
       (read-only-mode -1)
       ,@body
       (when ,read-only
         (read-only-mode 1)))))

(defmacro with-closest-entry (varname &rest body)
  (declare (indent 1) (debug t))
  `(let ((,varname (password-store-mode-closest-entry)))
     (if ,varname
         ,@body
       (message "No entry at point"))))

(defmacro save-point (&rest body)
  "Evaluate BODY and restore the point.
Similar to `save-excursion' but only restore the point."
  (declare (indent 0) (debug t))
  (let ((point (make-symbol "point")))
    `(let ((,point (point)))
       ,@body
       (goto-char (min ,point (point-max))))))

(defun password-store-mode--get-entries ()
  (password-store-list))

(provide 'password-store-mode)
;;; password-store-mode.el ends here
