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
(require 'url-util)
(require 'popup nil t)
(require 'undo-tree nil t)

(defgroup right-click-context ()
  "Right Click Context menu"
  :group 'convenience)

(defcustom right-click-context-interface 'popup-el
  "Menu interface for Right Click Context menu."
  :type 'function)

(defcustom right-click-context-mode-lighter " RightClick"
  "Lighter displayed in mode line when `right-click-context-mode' is enabled."
  :type 'string)

(defvar right-click-context-mode-map nil
  "Keymap used in right-click-context-mode.")

(unless right-click-context-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-3>") 'right-click-context-menu)
    (setq right-click-context-mode-map map)))

(defcustom right-click-context-global-menu-tree
  '(;;("Undo" :call (if (fboundp 'undo-tree-undo) (undo-tree-undo) (undo-only)))
    ;;("Redo" :call (if (fboundp 'undo-tree-redo) (undo-tree-redo)) :if (and (fboundp 'undo-tree-redo) (undo-tree-node-previous (undo-tree-current buffer-undo-tree))))
    ("Copy" :call (kill-ring-save beg end) :if (use-region-p))
    ("Cut"  :call (kill-region beg end) :if (and (use-region-p) (not buffer-read-only)))
    ("Paste" :call (yank) :if (not buffer-read-only))
    ("Select Region"
     ("All"  :call (mark-whole-buffer) :if (not (use-region-p)))
     ("Word" :call (mark-word))
     ("Paragraph" :call (mark-paragraph))
     ;("Rectangle" :call rectangle-mark-mode)
     )
    ("Text Convert"
     ("Downcase"   :call (downcase-region beg end))
     ("Upcase"     :call (upcase-region beg end))
     ("Capitalize" :call (capitalize-region beg end))
     ("URL Encode" :call (right-click-context--process-region beg end 'url-encode-url))
     ("URL Decode" :call (right-click-context--process-region beg end 'right-click-context--url-decode))
     ("Comment Out" :call comment-dwim))
    ("Go To"
     ("Top"    :call (goto-char (point-min)))
     ("Bottom" :call (goto-char (point-max)))
     ("Directory" :call (find-file default-directory)))
    ("Describe Character" :call (describe-char (point)) :if (not (use-region-p))))
  "Right Click Context menu."
  :type 'list)

(defun right-click-context--build-menu-for-popup-el (tree)
  "Build right click menu for `popup.el' from `TREE'."
  (cl-loop
   for n from 0
   for (name . l) in tree
   if (not (stringp name)) do (error (format "Invalid tree. (%d-th elm)" n))
   if (or (null (plist-get l :if)) (eval (plist-get l :if)))
   if (listp (car l)) collect (cons name (right-click-context--build-menu-for-popup-el l))
   else collect (popup-make-item name :value (plist-get l :call))))

(defvar right-click-context-local-menu-tree nil
  "Buffer local Right Click Menu.")
(make-variable-buffer-local 'right-click-context-local-menu-tree)

(defun right-click-context--menu-tree ()
  "Return right click menu tree."
  (cond ((and (symbolp right-click-context-local-menu-tree) (fboundp right-click-context-local-menu-tree)) (funcall right-click-context-local-menu-tree))
        (right-click-context-local-menu-tree right-click-context-local-menu-tree)
        (:else right-click-context-global-menu-tree)))

(defun right-click-context--process-region (begin end callback &rest args)
  "Convert string in region(BEGIN to END) by `CALLBACK' function call with additional arguments `ARGS'."
  (let ((region-string (buffer-substring-no-properties begin end))
        result)
    (setq result (apply callback region-string args))
    (if (null result)
        (error "Convert Error")
      (delete-region begin end)
      (insert result)
      (set-mark begin))))

(defun right-click-context--url-decode (src-string)
  "Return URI decoded string from `SRC-STRING'."
  (decode-coding-string (url-unhex-string (url-encode-url src-string)) 'utf-8))

;;;###autoload
(define-minor-mode right-click-context-mode
  "Minor mode for enable Right Click Context menu."
  :lighter right-click-context-mode-lighter
  :global t
  :require 'right-click-context
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
