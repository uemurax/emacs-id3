;;; emacs-id3.el - Emacs ID3 utility
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; License: GPL3
;; Time-stamp: <2016-02-15 05:26:55 tuemura>
;;
;;; Code:

(defgroup id3 nil
  "Predefined configurations for `emacs-id3.el'.")

(defcustom id3-frame-deliminator ?=
  "Deliminator of ID3 frame string."
  :group 'id3
  :type 'char)

;;;###autoload
(defun id3-frame-at-point (&optional point)
  "Return (KEY . VALUE) pair of ID3 frame at POINT."
  (let ((point (or point (point)))
        start end)
    (save-excursion
      (move-beginning-of-line nil)
      (setq start (point))
      (move-end-of-line nil)
      (setq end (point)))
    (and (>= (- end start) 5)
         (equal (char-after (+ start 4)) id3-frame-deliminator)
         (cons (buffer-substring start (+ start 4))
               (buffer-substring (+ start 5) end)))))

(defcustom id3-filename-prefix "IDv2 tag info for "
  "Prefix string of filename description."
  :group 'id3
  :type 'string)

(defun id3-filename-at-point (&optional point)
  "Filename at point."
  (let ((point (or point (point)))
        start end
        (len (length id3-filename-prefix)))
    (save-excursion
      (move-beginning-of-line nil)
      (setq start (point))
      (move-end-of-line nil)
      (setq end (point)))
    (and (equal (buffer-substring start (+ start len)) id3-filename-prefix)
         (buffer-substring (+ start len) end))))

(defun id3-parse (&optional start end)
  "Parse ID3 frames and file names."
  (let ((start (or start (point-min)))
        (end (or end (point-max)))
        (args nil))
    (save-excursion
      (goto-char start)
      (condition-case e
          (while (< (point) end)
            (let ((frame (id3-frame-at-point)))
              (if frame
                  (when args
                    (push frame (car args)))
                (let ((file (id3-filename-at-point)))
                  (when file
                    (push (list file) args)))))
            (next-logical-line))
        (error nil)))
    args))

(defun id3-make-mid3v2-options (lst)
  (mapconcat (lambda (x)
               (if (consp x)
                   (format "--%s %S" (car x) (cdr x))
                 (format "%S" x)))
             lst " "))

(defun id3-run-mid3v2 (&rest args)
  (mapc (lambda (x)
          (shell-command (concat "mid3v2"
                                 " "
                                 (id3-make-mid3v2-options x))))
        args))

;;;###autoload
(defun id3-write-with-mid3v2 (&optional start end)
  "Write ID3 tag using `mid3v2'."
  (interactive (if (region-active-p)
                   (list (region-beginning)
                         (region-end))
                 (list nil nil)))
  (apply #'id3-run-mid3v2 (id3-parse start end)))

(defcustom id3-mid3v2-buffer-name "*mid3v2 output* - %s"
  "Output buffer name for `mid3v2'."
  :group 'id3
  :type 'string)

;;;###autoload
(defun id3-read-with-mid3v2 (file &optional output-buffer)
  "Read ID3 tag using `mid3v2'."
  (interactive "fFind file: \nP")
  (let ((output-buffer (get-buffer-create
                        (cond ((or (stringp output-buffer)
                                   (bufferp output-buffer))
                               output-buffer)
                              (output-buffer (current-buffer))
                              (t (format id3-mid3v2-buffer-name file))))))
    (let ((str (with-temp-buffer
                 (shell-command (format "mid3v2 %S" file) t)
                 (delete-trailing-whitespace (point-min) (point-max))
                 (buffer-string))))
      (switch-to-buffer output-buffer)
      (insert str))))

(provide 'emacs-id3)

;;; emacs-id3.el ends here.
