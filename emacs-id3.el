;;; emacs-id3.el - Emacs ID3 utility
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; License: GPL3
;; Time-stamp: <2016-02-15 09:23:22 tuemura>
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

(defcustom id3-frame-spec-list
  ;; From <https://en.wikipedia.org/wiki/ID3>
  '(("AENC" . "Audio encryption")
    ("APIC" . "Attached picture")
    ("COMM" . "Comments")
    ("COMR" . "Commercial frame")
    ("ENCR" . "Encryption method registration")
    ("EQUA" . "Equalization")
    ("ETCO" . "Event timing codes")
    ("GEOB" . "General encapsulated object")
    ("GRID" . "Group identification registration")
    ("IPLS" . "Involved people list")
    ("LINK" . "Linked information")
    ("MCDI" . "Music CD identifier")
    ("MLLT" . "MPEG location lookup table")
    ("OWNE" . "Ownership frame")
    ("PRIV" . "Private frame")
    ("PCNT" . "Play counter")
    ("POPM" . "Popularimeter")
    ("POSS" . "Position synchronisation frame")
    ("RBUF" . "Recommended buffer size")
    ("RVAD" . "Relative volume adjustment")
    ("RVRB" . "Reverb")
    ("SYLT" . "Synchronized lyric/text")
    ("SYTC" . "Synchronized tempo codes")
    ("TALB" . "Album/Movie/Show title")
    ("TBPM" . "Beats per minute (BPM)")
    ("TCOM" . "Composer")
    ("TCON" . "Content type")
    ("TCOP" . "Copyright message")
    ("TDAT" . "Date")
    ("TDLY" . "Playlist delay")
    ("TENC" . "Encoded by")
    ("TEXT" . "Lyricist/Text writer")
    ("TFLT" . "File type")
    ("TIME" . "Time")
    ("TIT1" . "Content group description")
    ("TIT2" . "Title/songname/content description")
    ("TIT3" . "Subtitle/Description refinement")
    ("TKEY" . "Initial key")
    ("TLAN" . "Language(s)")
    ("TLEN" . "Length")
    ("TMED" . "Media type")
    ("TOAL" . "Original album/movie/show title")
    ("TOFN" . "Original filename")
    ("TOLY" . "Original lyricist(s)/text writer(s)")
    ("TOPE" . "Original artist(s)/performer(s)")
    ("TORY" . "Original release year")
    ("TOWN" . "File owner/licensee")
    ("TPE1" . "Lead performer(s)/Soloist(s)")
    ("TPE2" . "Band/orchestra/accompaniment")
    ("TPE3" . "Conductor/performer refinement")
    ("TPE4" . "Interpreted, remixed, or otherwise modified by")
    ("TPOS" . "Part of a set")
    ("TPUB" . "Publisher")
    ("TRCK" . "Track number/Position in set")
    ("TRDA" . "Recording dates")
    ("TRSN" . "Internet radio station name")
    ("TRSO" . "Internet radio station owner")
    ("TSIZ" . "Size")
    ("TSRC" . "International Standard Recording Code (ISRC)")
    ("TSSE" . "Software/Hardware and settings used for encoding")
    ("TYER" . "Year")
    ("TXXX" . "User defined text information frame")
    ("UFID" . "Unique file identifier")
    ("USER" . "Terms of use")
    ("USLT" . "Unsynchronized lyric/text transcription")
    ("WCOM" . "Commercial information")
    ("WCOP" . "Copyright/Legal information")
    ("WOAF" . "Official audio file webpage")
    ("WOAR" . "Official artist/performer webpage")
    ("WOAS" . "Official audio source webpage")
    ("WORS" . "Official internet radio station homepage")
    ("WPAY" . "Payment")
    ("WPUB" . "Publishers official webpage")
    ("WXXX" . "User defined URL link frame")
    ("ASPI" . "Audio seek point index")
    ("EQU2" . "Equalisation")
    ("RVA2" . "Relative volume adjustment")
    ("SEEK" . "Seek frame")
    ("SIGN" . "Signature frame")
    ("TDEN" . "Encoding time")
    ("TDOR" . "Original release time")
    ("TDRC" . "Recording time")
    ("TDRL" . "Release time")
    ("TDTG" . "Tagging time")
    ("TIPL" . "Involved people list")
    ("TMCL" . "Musician credits list")
    ("TMOO" . "Mood")
    ("TPRO" . "Produced notice")
    ("TSOA" . "Album sort order")
    ("TSOP" . "Performer sort order")
    ("TSOT" . "Title sort order")
    ("TSST" . "Set subtitle"))
  "ID3 frame list."
  :group 'id3
  :type '(alist :key-type string
                :value-type string))

(defun id3-set-tag (key value &optional all)
  (interactive (let ((f (id3-frame-at-point)))
                 (if f
                     (list (car f)
                           (read-string "Value: " (cdr f))
                           current-prefix-arg)
                   (list (substring (completing-read "Tag: "
                                                     (mapcar (lambda (x)
                                                               (format "%s - %s"
                                                                       (car x) (cdr x)))
                                                             id3-frame-spec-list))
                                    0 4)
                         (read-string "Value: ")
                         current-prefix-arg))))
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
")))))

(dolist (v '(("C-c C-c" . id3-write-with-mid3v2-and-quit)
             ("C-c C-k" . quit-window)
             ("C-c C-n" . id3-next-file)
             ("C-c C-p" . id3-previous-file)
             ("C-c C-t" . id3-set-tag)))
  (define-key id3-edit-mode-map (kbd (car v)) (cdr v)))

(provide 'emacs-id3)

;;; emacs-id3.el ends here.
