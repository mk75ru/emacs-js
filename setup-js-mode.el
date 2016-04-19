;;; setup-js-mode.el --- JS-mode Setup                -*- lexical-binding: t; -*-

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

;; This file is a helper for setting up a good environment for editing JS files.
;; Some of of the settings below are specific to Företagsplatsen, but most of it
;; should be generic and reusable.

;;; Code:

(require 'js2-mode)
(require 'js2-refactor)
(require 'amd-mode)
(require 'tern)
(require 'widgetjs-mode)
(require 'company-tern)
(require 'flycheck)
(require 'grunt)
(require 'xref-js2)

(add-hook 'js-mode-hook #'setup-js-buffer)

(defun setup-js-buffer ()
  (setq mode-name "JS")
  (company-mode 1)
  (tern-mode 1)
  (flycheck-mode 1)
  (js2-minor-mode 1)
  (js2-refactor-mode 1)
  (amd-mode 1)
  (widgetjs-mode 1)

  ;; add xref-js2 support
  (add-hook 'xref-backend-functions #'xref-js2--xref-backend nil t)

  (setq-local compile-command "grunt")

  ;; we use tabs in JS files
  (setq tab-width 4)
  (setq indent-tabs-mode t))

;; We have JS files in Scripts directories, ignore that
(add-to-list 'xref-js2-ignored-dirs "Scripts")

;; tern will override js2r keybindings...
(define-key tern-mode-keymap (kbd "C-c C-r") nil)

;; ... and xref.
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

(js2r-add-keybindings-with-prefix "C-c C-r")

(define-key js-mode-map (kbd "M-.") nil)
(define-key js-mode-map (kbd "C-c C-j") nil)

(define-key amd-mode-map (kbd "C-c C-a") #'amd-initialize-makey-group)

;; eslint parser executable can be overridden in some projects but marked as
;; risky, so silence that.
(put 'flycheck-javascript-eslint-executable 'risky-local-variable nil)

(defun kill-tern-process ()
  "Kill the tern process if any.
The process will be restarted.  This is useful if tern becomes
unreachable."
  (interactive)
  (delete-process "Tern"))

(add-to-list 'company-backends 'company-tern)

;; paredit-like commands for JS
(define-key js2-mode-map (kbd "<C-right>") #'js2r-forward-slurp)
(define-key js2-mode-map (kbd "<C-left>") #'js2r-forward-barf)
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)
(define-key js2-mode-map (kbd "M-S") #'js-smart-split)

;;; Convenience functions

(defun js-smart-split ()
  "Split the string or var declaration at point."
  (interactive)
  (let ((node (js2-node-at-point)))
    (cond ((js2-string-node-p node) (js2r-split-string))
          (t (js2r-split-var-declaration)))))

(defun mdn-search (searchString)
  "Open a browser on the MDN page for SEARCHSTRING."
  (interactive (list (read-string "Search: " (current-thing))))
  (browse-url (format "https://developer.mozilla.org/en-US/search?q=%s&topic=js" searchString)))

(define-key js-mode-map (kbd "C-c m") #'mdn-search)

;;; modified version of js-proper-indentation that works with FTGP rules

;; TODO: Commit a proper fix to Emacs.
(defun js--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((eq (char-after) ?#) 0)
          ((save-excursion (js--beginning-of-macro)) 4)
          ;; Indent array comprehension continuation lines specially.
          ((let ((bracket (nth 1 parse-status))
                 beg)
             (and bracket
                  (not (js--same-line bracket))
                  (setq beg (js--indent-in-array-comp bracket))
                  ;; At or after the first loop?
                  (>= (point) beg)
                  (js--array-comp-indentation bracket beg))))
          ((js--ctrl-statement-indentation))
          ((js--multi-line-declaration-indentation))
          ((nth 1 parse-status)
           ;; A single closing paren/bracket should be indented at the
           ;; same level as the opening statement. Same goes for
           ;; "case" and "default".
           (let ((same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p (js--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                 (progn ; nothing following the opening paren/bracket
                   (skip-syntax-backward " ")
                   (when (eq (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   (let* ((in-switch-p (unless same-indent-p
                                         (looking-at "\\_<switch\\_>")))
                          (same-indent-p (or same-indent-p
                                             (and switch-keyword-p
                                                  in-switch-p)))
                          (indent
                           (cond (same-indent-p
                                  (current-column))
                                 (continued-expr-p
                                  (+ (current-column) (* 2 js-indent-level)
                                     js-expr-indent-offset))
                                 (t
                                  (+ (current-column) js-indent-level
                                     (pcase (char-after (nth 1 parse-status))
                                       (?\( js-paren-indent-offset)
                                       (?\[ js-square-indent-offset)
                                       (?\{ js-curly-indent-offset)))))))
                     (if in-switch-p
                         (+ indent js-switch-indent-offset)
                       indent)))
               ;; If there is something following the opening
               ;; paren/bracket, everything else should be indented at
               ;; the same level.
               ; MODIFIED VERSION
               (if same-indent-p
                   (progn
                     (back-to-indentation)
                     (current-column))
                 (progn
                   (back-to-indentation)
                   (+ (current-column) js2-basic-offset)))
               ; COMMENTED ORIGINAL VERSION
               ;; (unless same-indent-p
               ;;   (forward-char)
               ;;   (skip-chars-forward " \t"))
               ;; (current-column)
               )))

          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t 0))))


(provide 'setup-js-mode)
;;; setup-js-mode.el ends here
