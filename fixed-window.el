;;; fixed-window --- open file with a fixed width window

;; Copyright (C) 2019- blue0513

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: blue0513
;; URL: https://github.com/blue0513/fixed-window
;; Version: 0.1.0

;;; Commentary:

;; Edit your init.el
;;
;; (require 'fixed-window)
;;

;;; Code:

(defvar fixed-window nil)

(defcustom fixed-window-disable-switch t
  "Disable to switch to the fixed-window by `other-window'."
  :type 'boolean
  :group 'fixed-window)

(defcustom fixed-window-disable-truncate nil
  "Disable to truncate in the fixed-window."
  :type 'boolean
  :group 'fixed-window)

(defcustom fixed-window-dedicated t
  "Make fixed-window dedicated."
  :type 'boolean
  :group 'fixed-window)

(defcustom fixed-window-width-ratio 0.85
  "Width ratio of fixed-window."
  :type 'float
  :group 'fixed-window)

(defun fw--delete-fixed-window-if-needed ()
  (if (and
       (member fixed-window (window-list))
       (windowp fixed-window))
      (progn
	(delete-window fixed-window)
	(setq fixed-window nil))))

(defun fw--open-fixed-window-with-file (buffer-name filename)
  (with-current-buffer buffer-name
    (find-file filename)
    (set-window-parameter fixed-window 'no-other-window fixed-window-disable-switch)
    (set-window-dedicated-p fixed-window fixed-window-dedicated)
    (setq truncate-partial-width-windows fixed-window-disable-truncate)
    (setq window-size-fixed 'width)
    (setq fixed-window (selected-window))))

(defun fw--validations (filename ratio)
  (unless (file-exists-p filename) (error "No valid file found"))
  (if (and ratio (numberp ratio)) (error "Ratio must be number")))

(defun fixed-window-goto ()
  (interactive)
  ;; Hack
  (set-window-parameter fixed-window 'no-other-window nil)
  (selected-window)
  (other-window -1)
  (set-window-parameter fixed-window 'no-other-window t))

(defun fixed-window-delete ()
  (interactive)
  (fw--delete-fixed-window-if-needed))

(defun fixed-window-create (filename &optional ratio)
  (interactive "f")
  (fw--validations filename ratio)
  (fw--delete-fixed-window-if-needed)
  (let* ((default-ratio fixed-window-width-ratio)
	 (buffer-name " *temp-fixed-width-buffer")
	 (width-ratio (if (null ratio) default-ratio ratio))
	 (width (round (* (window-width) width-ratio))))
    (split-window-horizontally width)
    (switch-to-buffer (get-buffer-create buffer-name) t t)
    (fw--open-fixed-window-with-file buffer-name filename)))

;; * provide

(provide 'fixed-window)

;;; fixed-window.el ends here
