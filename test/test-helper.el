;; test-helper.el --- Test helpers for sift.el

;; Copyright (C) 2015, 2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;; Homepage: https://github.com/nlamirault/sift.el

;;; License:

;; This file is NOT part of GNU Emacs.

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

;;; Code:

(require 'ansi)
(require 'cl) ;; http://emacs.stackexchange.com/questions/2864/symbols-function-definition-is-void-cl-macroexpand-all-when-trying-to-instal
(require 'ert)
(require 'f)
(require 'undercover)

(setq debugger-batch-max-lines (+ 50 max-lisp-eval-depth)
      debug-on-error t)

(defvar username (getenv "HOME"))

(defconst sift-testsuite-dir
  (f-parent (f-this-file))
  "The testsuite directory.")

(defconst sift-source-dir
  (f-parent sift-testsuite-dir)
  "The sift.el source directory.")

(defconst sift-sandbox-path
  (f-expand "sandbox" sift-testsuite-dir)
  "The sandbox path for sift.")

(defun cleanup-load-path ()
  "Remove home directory from 'load-path."
  (message (ansi-green "[sift] Cleanup path"))
  (mapc #'(lambda (path)
            (when (string-match (s-concat username "/.emacs.d") path)
              (message (ansi-yellow "Suppression path %s" path))
              (setq load-path (delete path load-path))))
        load-path))

(defun load-unit-tests (path)
  "Load all unit test from PATH."
  (message (ansi-green "[sift] Execute unit tests %s"
                       path))
  (dolist (test-file (or argv (directory-files path t "-test.el$")))
    (load test-file nil t)))


(defun load-library (file)
  "Load current library from FILE."
  (let ((path (s-concat sift-source-dir file)))
    (message (ansi-yellow "[sift] Load library from %s" path))
    (undercover "*.el" (:exclude "*-test.el"))
    (require 'sift path)))


(defmacro with-test-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(unwind-protect
       (condition-case nil ;ex
           (let ((default-directory sift-source-dir))
             ;; (unless (f-dir? sift-sandbox-path)
             ;;   (f-mkdir sift-sandbox-path))
             (cleanup-load-path)
             (load-library "/sift.el")
             ,@body)
         ;; (f-delete sift-sandbox-path :force)))
         )))
         ;; (error
         ;;  (message (ansi-red "[sift] Error during unit tests : %s" ex))))))

(provide 'test-helper)
;;; test-helper.el ends here
