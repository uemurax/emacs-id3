;;; emacs-id3.el - Emacs ID3 utility
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; License: GPL3
;; Time-stamp: <2016-02-15 09:01:12 tuemura>
;;
;;; Code:

(defgroup id3 nil
  "Predefined configurations for `emacs-id3.el'.")

(defcustom id3-frame-deliminator ?=
  "Deliminator of ID3 frame string."
  :group 'id3
  :type 'char)

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
    (and (equal (buffer-substring start (min (point-max) (+ start len))) id3-filename-prefix)
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
(defun id3-read-with-mid3v2 (&optional output-buffer &rest files)
  "Read ID3 tag using `mid3v2'."
  (let ((output-buffer (get-buffer-create
                        (cond ((or (stringp output-buffer)
                                   (bufferp output-buffer))
                               output-buffer)
                              (output-buffer (current-buffer))
                              (t (format id3-mid3v2-buffer-name file))))))
    (let ((str (with-temp-buffer
                 (shell-command (concat "mid3v2 "
                                        (mapconcat (lambda (file)
                                                     (format "%S" file))
                                                   files " "))
                                t)
                 (delete-trailing-whitespace (point-min) (point-max))
                 (buffer-string))))
      (with-current-buffer output-buffer
        (insert str)))))

;; ----------------------------------------------------------------
;; ID3 edit mode
;; ----------------------------------------------------------------

(define-derived-mode id3-edit-mode text-mode "ID3"
  "Major mode to edit ID3 tag.")

(defun id3-write-with-mid3v2-and-quit ()
  (interactive)
  (id3-write-with-mid3v2)
  (quit-window))

(defun id3-next-file (n)
  (interactive "p")
  (when (id3-filename-at-point)
    (move-end-of-line nil))
  (re-search-forward (concat "^" id3-filename-prefix) nil 'noerror n)
  (move-beginning-of-line nil))

(defun id3-previous-file (n)
  (interactive "p")
  (when (id3-filename-at-point)
    (move-beginning-of-line nil))
  (re-search-backward (concat "^" id3-filename-prefix) nil 'noerror n)
  (move-beginning-of-line nil))

(defun id3-current-file ()
  (interactive)
  (if (id3-filename-at-point)
      (move-beginning-of-line)
    (id3-previous-file 1)))

(defun id3-for-current-file (f)
  "Move to the current file and call function F with
argument the end of the current file."
  (let (end)
    (save-excursion
      (id3-next-file 1)
      (setq end (point))
      (id3-previous-file 1)
      (funcall f end))))

(defun id3-for-each-file (f)
  "For each file, move to the file and call function F with
argument the end of the file."
  (save-excursion
    (goto-char (point-min))
    (unless (id3-filename-at-point)
      (id3-next-file 1))
    (while (< (point) (point-max))
      (id3-for-current-file f)
      (id3-next-file 1))))

(defun id3-set-tag (key value &optional all)
  (interactive (let ((f (id3-frame-at-point)))
                 (if f
                     (list (car f)
                           (read-string "Value: " (cdr f))
                           current-prefix-arg)
                   (list (read-string "Tag: ")
                         (read-string "Value: ")
                         current-prefix-arg))))
  (if key
      (funcall (if all 'id3-for-each-file 'id3-for-current-file)
               (lambda (end)
                 (if (re-search-forward (concat "^" key "=") end 'noerror 1)
                     (let ((a (point))
                           (b (save-excursion
                                (move-end-of-line nil)
                                (point))))
                       (delete-region a b)
                       (insert value))
                   (insert key "=" value "
"))))
    (message "No tag at that point.")))

(dolist (v '(("C-c C-c" . id3-write-with-mid3v2-and-quit)
             ("C-c C-k" . quit-window)
             ("C-c C-n" . id3-next-file)
             ("C-c C-p" . id3-previous-file)
             ("C-c C-t" . id3-set-tag)))
  (define-key id3-edit-mode-map (kbd (car v)) (cdr v)))

(provide 'emacs-id3)

;;; emacs-id3.el ends here.
