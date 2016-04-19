;;; setup.el --- Setup the JS environment            -*- lexical-binding: t; -*-

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

;; Setup the JS environment: install required packages if necessary, and setup
;; JS buffers.

;;; Code:

(require 'package)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(let ((packages-to-load '(js2-mode
                          js2-refactor
                          amd-mode
                          tern
                          projectile
                          company-tern
                          flycheck
                          grunt
                          xref-js2)))
  (dolist (package packages-to-load)
    (when (not (package-installed-p package))
      (package-install package))))

(load-file "widgetjs/widgetjs-mode.el")
(load-file "setup-js-mode.el")

(provide 'setup)
;;; setup.el ends here
