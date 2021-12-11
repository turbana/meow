;;; meow-helpers.el --- Meow Helpers for define keybinding  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Define custom keys in normal map with function `meow-normal-define-key'.
;; Define custom keys in global leader map with function `meow-leader-define-key'.
;; Define custom keys in leader map for specific mode with function `meow-leader-define-mode-key'.

;;; Code:

(require 'cl-lib)

(require 'meow-util)
(require 'meow-var)
(require 'meow-keymap)

(defun meow-leader-define-key (&rest args)
  "Define key for Leader.

Usage:
  (meow-leader-define-key
   '(\"h\" . hs-toggle-hiding))
Optional argument ARGS key definitions."
  (mapcar (lambda (key-def)
            (define-key meow-leader-keymap
              (kbd (car key-def))
              (meow--parse-def (cdr key-def))))
          args))

(defun meow-local-leader-define-key (mode &rest args)
  "Define key for Local Leader under MODE.

MODE-MAP should be a quoted keymap where the local leader keymap will be
defined. `meow-local-leader-prefix' must be set prior to calling.

Usage:
  (meow-local-leader-define-key 'emacs-lisp-mode
   '(\"h\" . hs-toggle-hiding))
Optional argument ARGS key definitions."
  (declare (indent 1))
  (let ((keymap-sym (intern (format "%s-map" mode))))
    (if (and (boundp keymap-sym) keymap-sym)
        (meow--local-leader-define-key-impl keymap-sym args)
      (let ((mode-sans-mode (intern (replace-regexp-in-string
                                     "-mode$" "" (symbol-name mode)))))
        (dolist (feature-guess (list mode mode-sans-mode))
          (with-eval-after-load feature-guess
            (meow--local-leader-define-key-impl keymap-sym args)))))))

(defun meow--local-leader-define-key-impl (mode-map keys)
  "Implementation function for `meow-local-leader-define-key'."
  (declare (indent 1))
  (unless meow-local-leader-prefix
    (user-error "`meow-local-leader-prefix' must be set to a prefix key."))
  (let ((keymap (assoc-default mode-map meow--local-leader-maps)))
    (unless keymap
      (setq keymap (make-sparse-keymap))
      (define-key (symbol-value mode-map) (kbd meow-local-leader-prefix) keymap)
      (push (cons mode-map keymap) meow--local-leader-maps))
    (mapcar (lambda (key-def)
              (define-key keymap
                (kbd (car key-def))
                (meow--parse-def (cdr key-def))))
            keys)))

(defun meow-normal-define-key (&rest args)
  "Define key for normal state.

Usage:
  (meow-normal-define-key
   '(\"@\" . hs-toggle-hiding))
Optional argument ARGS key definitions."
  (mapcar (lambda (key-def)
            (define-key meow-normal-state-keymap
              (kbd (car key-def))
              (meow--parse-def (cdr key-def))))
          args))

(defun meow-motion-overwrite-define-key (&rest args)
  "Define key for motion state."
  (mapc (lambda (key-def)
          (define-key meow-motion-state-keymap
            (kbd (car key-def))
            (meow--parse-def (cdr key-def))))
        args)
  (cl-loop for arg in args do
           (add-to-list 'meow--motion-overwrite-keys (car arg))))

(defun meow-setup-line-number ()
  (add-hook 'display-line-numbers-mode-hook #'meow--toggle-relative-line-number)
  (add-hook 'meow-insert-mode-hook #'meow--toggle-relative-line-number))

(defun meow-setup-indicator ()
  "Setup indicator appending the return of function
`meow-indicator' to the modeline.

This function should be called after you setup other parts of the mode-line
 and will work well for most cases.

If this function is not enough for your requirements,
use `meow-indicator' to get the raw text for indicator
and put it anywhere you want."
  (unless (cl-find '(:eval (meow-indicator)) mode-line-format :test 'equal)
    (setq-default mode-line-format (append '((:eval (meow-indicator))) mode-line-format))))

(provide 'meow-helpers)
;;; meow-helpers.el ends here
