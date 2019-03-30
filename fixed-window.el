(defvar fixed-window nil)
(defvar fixed-window-to-go t)
(defvar fixed-window-truncate t)
(defvar fixed-window-dedicated t)

(defun delete-fixed-window-if-needed ()
  (if (and
       (member fixed-window (window-list))
       (windowp fixed-window))
      (progn
	(delete-window fixed-window)
	(setq fixed-window nil))))

(defun open-fixed-window-with-file (buffer-name filename)
  (with-current-buffer buffer-name
    (find-file filename)
    (set-window-parameter fixed-window 'no-other-window (not fixed-window-to-go))
    (set-window-dedicated-p fixed-window fixed-window-dedicated)
    (setq truncate-partial-width-windows (not fixed-window-truncate))
    (setq window-size-fixed 'width)
    (setq fixed-window (selected-window))))

(defun fixed-window-validations (filename ratio)
  (unless (file-exists-p filename) (error "No valid file found"))
  (if (and ratio (numberp ratio)) (error "Ratio must be number")))

(defun goto-fixed-window ()
  (interactive)
  ;; Hack
  (set-window-parameter fixed-window 'no-other-window nil)
  (selected-window)
  (other-window -1)
  (set-window-parameter fixed-window 'no-other-window t))

(defun delete-fixed-window ()
  (interactive)
  (delete-fixed-window-if-needed))

(defun hide-fixed-window ()
  (interactive)
  (message "TODO"))

(defun create-fixed-window (filename &optional ratio)
  (interactive "f")
  (fixed-window-validations filename ratio)
  (delete-fixed-window-if-needed)
  (let* ((default-ratio 0.85)
	 (buffer-name " *temp-fixed-width-buffer")
	 (width-ratio (if (null ratio) default-ratio ratio))
	 (width (round (* (window-width) width-ratio))))
    (split-window-horizontally width)
    (switch-to-buffer (get-buffer-create buffer-name) t t)
    (open-fixed-window-with-file buffer-name filename)))
