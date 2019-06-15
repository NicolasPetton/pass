;;; pass-tests.el --- Tests for pass.el

;; Copyright (C) 2013-2019 Damien Cassou

;; Author: Damien Cassou <damien.cassou@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Tests for pass-mode.el

;;; Code:

(require 'ert)
(require 'pass)
(require 'password-store)

(ert-deftest pass-goto-entry-tests ()
  "Tests for `pass-goto-entry`."
  ;; Test requirements:
  ;; 1) The pass executable must exist
  ;; 2) The password storage must be initialized with a valid public key (pass init GPG-ID)
  (skip-unless (and (executable-find password-store-executable)
                    (file-exists-p (expand-file-name ".password-store"
                                                     (getenv "HOME")))))
  (let ((entries '("pass-goto-entry-test-1" "pass-goto-entry-test-2")))
    (pass)
    (unwind-protect
        (progn
          (dolist (entry entries)
            (password-store-generate entry)
            (pass-update-buffer)
            ;; Users can jump into an existing entry...
            (should (pass-goto-entry entry)))
          ;; ...but they cannot jump into non-existent ones
          (should-not (pass-goto-entry "pass-goto-entry-test-999")))
      ;; Delete created entries
      (dolist (entry entries)
        (password-store-remove entry))
      (pass-update-buffer))))
          

(provide 'pass-tests)

;;; pass-tests.el ends here
