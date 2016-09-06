;;; gulp.el --- Gulp task runner                     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: convenience

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

;;

;;; Code:

(defun gulp ()
  (interactive)
  (let* ((tasks (gulp--get-tasks))
         (task (completing-read "Gulp task: " tasks)))
    (compile (format "gulp %s" task))))

(defun gulp--get-tasks ()
  (with-temp-buffer
    (shell-command "gulp --tasks-simple" (current-buffer))
    (split-string (buffer-string) "\n")))

(provide 'gulp)
;;; gulp.el ends here
