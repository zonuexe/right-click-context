;;; right-click-context.el --- Right Click Context menu

;; Copyright (C) 2016 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 8 May 2016
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "0.5") (popup "0.5"))
;; Keywords: mouse menu rightclick
;; Homepage: https://github.com/zonuexe/right-click-context

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;;     (right-click-context-mode 1)

;;; Code:
(require 'cl-lib)
(require 'popup nil t)
(require 'undo-tree nil t)

(defgroup right-click-context ()
  "Right Click Context menu"
  :group 'convenience)

(defcustom right-click-context-interface 'popup-el
  "Menu interface for Right Click Context menu.")

(defcustom right-click-context-mode-lighter " RightClick"
  "Lighter displayed in mode line when `right-click-context-mode' is enabled.")

(defvar right-click-context-mode-map nil
  "Keymap used in right-click-context-mode.")

(unless right-click-context-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-3>") 'right-click-context-menu)
    (setq right-click-context-mode-map map)))

(defcustom right-click-context-global-menu-tree
  '(;;("Undo" :call (if (fboundp 'undo-tree-undo) (undo-tree-undo) (undo-only)))
    ;;("Redo" :call (if (fboundp 'undo-tree-redo) (undo-tree-redo)) :if (and (fboundp 'undo-tree-redo) (undo-tree-node-previous (undo-tree-current buffer-undo-tree))))
    ("Copy" :call (kill-ring-save beg end))
    ("Cut"  :call (kill-region beg end))
    ("Paste" :call (yank))
    ("Select All" :call (mark-whole-buffer) :if (not (use-region-p)))
    ("Indent" :call #'indent-for-tab-command)
    ("Text Convert"
     ("downcase" :call (downcase-region beg end))
     ("upcase"   :call (upcase-region beg end))))
  "Right Click Context menu.")

(defun right-click-context--build-menu-for-popup-el (tree)
  "Build right click menu for `popup.el' from `TREE'."
  (cl-loop
   for n from 0
   for (name . l) in tree
   if (not (stringp name)) return (error (format "Invalid tree. (%d-th elm)" n))
   if (or (null (plist-get l :if)) (eval (plist-get l :if)))
   if (listp (car l)) collect (cons name (right-click-context--build-menu-for-popup-el l))
   else collect (popup-make-item name :value (plist-get l :call))))

(defvar right-click-context-local-menu-tree nil
  "Buffer local Right Click Menu.")
(make-variable-buffer-local 'right-click-context-local-menu-tree)

(defun right-click-context--menu-tree ()
  "Return right click menu tree."
  (cond
   ((fboundp right-click-context-local-menu-tree) (funcall right-click-context-local-menu-tree))
   (right-click-context-local-menu-tree right-click-context-local-menu-tree)
   (:else right-click-context-global-menu-tree)))

;;;###autoload
(define-minor-mode right-click-context-mode
  "Minor mode for enable Right Click Context menu."
  :lighter right-click-context-mode-lighter
  :global t
  right-click-context-mode-map
  :group 'right-click-context)

;;;###autoload
(defun right-click-context-menu ()
  "Open Right Click Context menu."
  (interactive)
  (let ((value (popup-cascade-menu (right-click-context--build-menu-for-popup-el (right-click-context--menu-tree))))
        beg end)
    (when value
      (when (region-active-p)
        (setq beg (region-beginning))
        (setq end (region-end)))
      (if (symbolp value)
          (call-interactively value t)
        (eval value)))))

(provide 'right-click-context)
;;; right-click-context.el ends here
