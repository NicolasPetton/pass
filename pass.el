;;; pass.el --- Major mode for password-store.el -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2017  Nicolas Petton & Damien Cassou

;; Author: Nicolas Petton <petton.nicolas@gmail.com>
;;         Damien Cassou <damien@cassou.me>
;; Version: 1.7
;; GIT: https://github.com/NicolasPetton/pass
;; Package-Requires: ((emacs "24") (password-store "0.1") (f "0.17"))
;; Created: 09 Jun 2015
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

;;; Code:
(require 'password-store)
(require 'imenu)
(require 'button)
(require 'f)

(defgroup pass '()
  "Major mode for password-store."
  :group 'password-store)

(defcustom pass-show-keybindings t
  "Whether the keybindings should be displayed in the pass buffer."
  :group 'pass
  :type 'boolean)

(defvar pass-buffer-name "*Password-Store*"
  "Name of the pass buffer.")

(defvar pass-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'pass-next-entry)
    (define-key map (kbd "p") #'pass-prev-entry)
    (define-key map (kbd "M-n") #'pass-next-directory)
    (define-key map (kbd "M-p") #'pass-prev-directory)
    (define-key map (kbd "k") #'pass-kill)
    (define-key map (kbd "s") #'isearch-forward)
    (define-key map (kbd "?") #'describe-mode)
    (define-key map (kbd "g") #'pass-update-buffer)
    (define-key map (kbd "i") #'pass-insert)
    (define-key map (kbd "I") #'pass-insert-generated)
    (define-key map (kbd "w") #'pass-copy)
    (define-key map (kbd "v") #'pass-view)
    (define-key map (kbd "r") #'pass-rename)
    (define-key map (kbd "RET") #'pass-view)
    (define-key map (kbd "q") #'pass-quit)
    map)
  "Keymap for `pass-mode'.")

(defface pass-mode-header-face '((t . (:inherit font-lock-keyword-face)))
  "Face for displaying the header of the pass buffer."
  :group 'pass)

(defface pass-mode-entry-face '((t . ()))
  "Face for displaying pass entry names."
  :group 'pass)

(defface pass-mode-directory-face '((t . (:inherit
                                          font-lock-function-name-face
                                          :weight
                                          bold)))
  "Face for displaying password-store directory names."
  :group 'pass)

(define-derived-mode pass-mode nil "Password-Store"
  "Major mode for editing password-stores.

\\{pass-mode-map}"
  (setq default-directory (password-store-dir))
  (setq-local imenu-generic-expression '((nil "├──\\ \\(.*\\)" 1)))
  (read-only-mode))

(defun pass-setup-buffer ()
  "Setup the password-store buffer."
  (pass-mode)
  (pass-update-buffer))

;;;###autoload
(defun pass ()
  "Open the password-store buffer."
  (interactive)
  (if (get-buffer pass-buffer-name)
      (progn
        (switch-to-buffer pass-buffer-name)
        (pass-update-buffer))
    (let ((buf (get-buffer-create pass-buffer-name)))
      (pop-to-buffer buf)
      (pass-setup-buffer))))

(defmacro pass--with-writable-buffer (&rest body)
  "Evaluate BODY as if the current buffer was not in `read-only-mode'."
  (declare (indent 0) (debug t))
  `(let ((inhibit-read-only t))
     ,@body))

(defmacro pass--save-point (&rest body)
  "Evaluate BODY and restore the point.
Similar to `save-excursion' but only restore the point."
  (declare (indent 0) (debug t))
  (let ((point (make-symbol "point")))
    `(let ((,point (point)))
       ,@body
       (goto-char (min ,point (point-max))))))

(defun pass-quit ()
  "Kill the buffer quitting the window."
  (interactive)
  (when (y-or-n-p "Kill all pass entry buffers? ")
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'pass-view-mode)
          (kill-buffer buf)))))
  (quit-window t))

(defun pass-next-entry ()
  "Move point to the next entry found."
  (interactive)
  (pass--goto-next #'pass-entry-at-point))

(defun pass-prev-entry ()
  "Move point to the previous entry."
  (interactive)
  (pass--goto-prev #'pass-entry-at-point))

(defun pass-next-directory ()
  "Move point to the next directory found."
  (interactive)
  (pass--goto-next #'pass-directory-at-point))

(defun pass-prev-directory ()
  "Move point to the previous directory."
  (interactive)
  (pass--goto-prev #'pass-directory-at-point))

(defmacro pass--with-closest-entry (varname &rest body)
  "Bound VARNAME to the closest entry before point and evaluate BODY."
  (declare (indent 1) (debug t))
  `(let ((,varname (pass-closest-entry)))
     (if ,varname
         (progn ,@body)
       (message "No entry at point"))))

(defun pass-rename (new-name)
  "Rename the entry at point to NEW-NAME."
  (interactive (list (read-string "Rename entry to: " (pass-closest-entry))))
  (pass--with-closest-entry entry
    (password-store-rename entry new-name)
    (pass-update-buffer)))

(defun pass-kill ()
  "Remove the entry at point."
  (interactive)
  (pass--with-closest-entry entry
    (when (yes-or-no-p (format "Do you want remove the entry %s? " entry))
      (password-store-remove entry)
      (pass-update-buffer))))

(defun pass-update-buffer ()
  "Update the current buffer contents."
  (interactive)
  (pass--save-point
    (pass--with-writable-buffer
      (delete-region (point-min) (point-max))
      (pass-display-data))))

(defun pass-insert (arg)
  "Insert an entry to the password-store.
The password is read from user input.

When called with a prefix argument ARG, visit the entry file."
  (interactive "P")
  (if arg
      (pass-insert-multiline)
    (progn
      (call-interactively #'password-store-insert)
      (pass-update-buffer))))

(defun pass-insert-multiline ()
  "This function behaves similarly to `pass -m'.
It creates an empty entry file, and visit it."
  (let ((entry (format "%s.gpg" (read-string "Password entry: ")))
        (default-directory (password-store-dir)))
    (make-directory (file-name-directory entry) t)
    (find-file (expand-file-name entry (password-store-dir)))))

(defun pass-insert-generated ()
  "Insert an entry to the password-store.
Use a generated password instead of reading the password from
user input."
  (interactive)
  (call-interactively #'password-store-generate)
  (pass-update-buffer))

(defun pass-view ()
  "Visit the entry at point."
  (interactive)
  (pass--with-closest-entry entry
    (password-store-edit entry)))

(defun pass-copy ()
  "Add the entry at point to kill ring."
  (interactive)
  (pass--with-closest-entry entry
    (password-store-copy entry)))

(defun pass-display-data ()
  "Display the password-store data into the current buffer."
  (let ((items (pass--tree)))
    (pass-display-header)
    (pass-display-item items)))

(defun pass-display-header ()
  "Display the header in to the current buffer."
  (insert "Password-store directory:")
  (put-text-property (point-at-bol) (point) 'face 'pass-mode-header-face)
  (insert " ")
  (pass--display-keybindings-toggle)
  (insert "\n\n")
  (when pass-show-keybindings
    (pass--display-keybindings '((pass-copy . "Copy password")
                                 (pass-view . "View entry")
                                 (pass-update-buffer . "Update")))
    (insert "\n")
    (pass--display-keybindings '((pass-insert . "Insert")
                                 (pass-next-entry . "Next")
                                 (describe-mode . "Help")))
    (insert "\n")
    (pass--display-keybindings '((pass-insert-generated . "Generate")
                                 (pass-prev-entry . "Previous")))
    (insert "\n")
    (pass--display-keybindings '((pass-rename . "Rename")
                                 (pass-next-directory . "Next dir")))
    (insert "\n")
    (pass--display-keybindings '((pass-kill . "Delete")
                                 (pass-prev-directory . "Previous dir")))
    (newline)
    (newline)))

(defun pass--display-keybindings-toggle ()
  "Display a button to toggle whether keybindings should be displayed."
  (let ((label (if pass-show-keybindings
                   "[Hide keybindings]"
                 "[Show keybindings]")))
    (insert-button label 'action #'pass--toggle-display-keybindings)))

(defun pass--toggle-display-keybindings (&rest _)
  "Toggle displaying the keybindings and update the buffer."
  (setq pass-show-keybindings (not pass-show-keybindings))
  (put pass-show-keybindings
       'customized-value
       (list (custom-quote (symbol-value pass-show-keybindings))))
  (pass-update-buffer))

(defun pass--display-keybindings (bindings)
  "Display the keybinding in each item of BINDINGS.
BINDINGS is an alist of bindings."
  (mapc (lambda (pair)
          (pass--display-keybinding (car pair) (cdr pair)))
        bindings))

(defun pass--display-keybinding (command label)
  "Insert the associated keybinding for COMMAND with LABEL."
  (insert (format "%8s %-12s \t "
                  (format "%s"
                          (propertize (substitute-command-keys
                                       (format "<\\[%s]>" (symbol-name command)))
                                      'face 'font-lock-constant-face))
                  label)))

(defun pass-display-item (item &optional indent-level)
  "Display the directory or entry ITEM into the current buffer.
If INDENT-LEVEL is specified, add enough spaces before displaying
ITEM."
  (unless indent-level (setq indent-level 0))
  (let ((directory (listp item)))
    (pass-display-item-prefix indent-level)
    (if directory
        (pass-display-directory item indent-level)
      (pass-display-entry item))))

(defun pass-display-entry (entry)
  "Display the password-store entry ENTRY into the current buffer."
  (let ((entry-name (f-filename entry)))
    (insert entry-name)
    (add-text-properties (point-at-bol) (point)
                         `(face pass-mode-entry-face pass-entry ,entry))
    (newline)))

(defun pass-display-directory (directory indent-level)
  "Display the directory DIRECTORY into the current buffer.

DIRECTORY is a list, its CAR being the name of the directory and its CDR
the entries of the directory.  Add enough spaces so that each entry is
indented according to INDENT-LEVEL."
  (let ((name (car directory))
        (items (cdr directory)))
    (insert name)
    (add-text-properties (point-at-bol) (point)
                         `(face pass-mode-directory-face pass-directory ,name))
    (newline)
    (dolist (item items)
      (pass-display-item item (1+ indent-level)))))

(defun pass-display-item-prefix (indent-level)
  "Display some indenting text according to INDENT-LEVEL."
  (dotimes (_ (max 0 (* (1- indent-level) 4)))
    (insert " "))
  (unless (zerop indent-level)
    (insert "├── ")))

(defun pass-entry-at-point ()
  "Return the `pass-entry' property at point."
  (get-text-property (point-at-eol) 'pass-entry))

(defun pass-directory-at-point ()
  "Return the `pass-directory' property at point."
  (get-text-property (point) 'pass-directory))

(defun pass-closest-entry ()
  "Return the closest entry in the current buffer, looking backward."
  (save-excursion
    (or (pass-entry-at-point)
        (unless (bolp)
          (forward-line -1)
          (pass-closest-entry)))))

(defun pass--goto-next (pred)
  "Move point to the next match of PRED."
  (forward-line)
  (while (not (or (eobp) (funcall pred)))
    (forward-line)))

(defun pass--goto-prev (pred)
  "Move point to the previous match of PRED."
  (forward-line -1)
  (while (not (or (bobp) (funcall pred)))
    (forward-line -1)))

(defun pass--tree (&optional subdir)
  "Return a tree of all entries in SUBDIR.
If SUBDIR is nil, return the entries of `(password-store-dir)'."
  (unless subdir (setq subdir ""))
  (let ((path (f-join (password-store-dir) subdir)))
    (if (f-directory? path)
        (unless (string= (f-filename subdir) ".git")
          (cons (f-filename path)
                (delq nil
                      (mapcar 'pass--tree
                              (f-entries path)))))
      (when (equal (f-ext path) "gpg")
        (password-store--file-to-entry path)))))

;;; major mode for viewing entries

(defvar pass-view-mask "·············"
  "Mask used to hide passwords.")

(defvar pass-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'pass-view-toggle-password)
    (define-key map (kbd "C-c C-w") #'pass-view-copy-password)
    map))

(defun pass-view-toggle-password ()
  "Enable or disable password hiding."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((buf-modified (buffer-modified-p)))
      (if (string= (get-text-property (point) 'display)
                   pass-view-mask)
          (pass-view-unmask-password)
        (pass-view-mask-password))
      (set-buffer-modified-p buf-modified))))

(defun pass-view-copy-password ()
  "Copy the password of the entry in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (copy-region-as-kill (point) (line-end-position))))

(defun pass-view-mask-password ()
  "Mask the password of the current buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (set-text-properties (point-min) (line-end-position)
                           `(display ,pass-view-mask)))))

(defun pass-view-unmask-password ()
  "Show the password in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (remove-text-properties (point-min) (line-end-position)
                            '(display nil))))

(defvar pass-view-font-lock-keywords '("^[^\s\n]+:" . 'font-lock-keyword-face)
  "Font lock keywords for ‘pass-view-mode’.")

(define-derived-mode pass-view-mode nil "Pass-View"
  "Major mode for viewing password-store entries.

\\{pass-view-mode-map}"
  (pass-view-toggle-password)
  (setq-local font-lock-defaults '(pass-view-font-lock-keywords))
  (font-lock-mode 1)
  (message
   (substitute-command-keys
    "Press <\\[pass-view-toggle-password]> to display & edit the password")))

(add-to-list 'auto-mode-alist '("\\.password-store/.*\\.gpg\\'" . pass-view-mode))

(provide 'pass)
;;; pass.el ends here
