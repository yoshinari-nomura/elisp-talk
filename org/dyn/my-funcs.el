;;; my-functions.el --- My private functions.

;; Author:  Yoshinari Nomura
;; Created: 2005-03-27

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO, ADDRESS, MEMO

(defvar mydocument-path (expand-file-name "~/prj/private/"))

(defvar todo-file-name "org/TODO.org")
(defvar memo-file-name "MEMO.org")
(defvar address-file-name "ADDRESS.org")

(defun find-mydoc-file-other-window (filename)
  (find-file-other-window
   (concat mydocument-path filename))
  (message ""))

(defun todo ()
  (interactive)
  (find-mydoc-file-other-window todo-file-name))

(defun address ()
  (interactive)
  (find-mydoc-file-other-window address-file-name))

(defun memo ()
  (interactive)
  (find-mydoc-file-other-window memo-file-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numbering

(defvar numbering-default-string "^[\t 　]*(\\( *[0-9]+\\))")

(defun numbering (&optional arg)
  (interactive)
  (let ((n 1))
    (save-excursion
      (setq numbering-default-string
            (read-from-minibuffer (format "Numbering format (%s):"
                                          numbering-default-string)
                                  numbering-default-string))
      (goto-char (point-min))
      (while (re-search-forward numbering-default-string nil t)
        (replace-match (format "%02d" n) nil nil nil 1)
        (setq n (1+ n)))
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sum up numbers in a string, region or rectangle.

(defun sum-string (string)
  (let ((str "") (sum 0))
    (mapc
     (lambda (c)
       (cond
        ((char-equal c ?,)) ;; skip comma
        ((string-match "[0-9.+-]" (char-to-string c))
         (setq str (format "%s%c" str c)))
        (t (setq sum (+ sum (string-to-number str)) str ""))))
     (concat string "X"))
    sum))

(defvar sum-money-regexp "\\([0-9,]+\\)円")

(defun sum-string-to-int (string)
  (sum-string string))

(defun sum-money-in-region (p1 p2)
  (interactive "r")
  (let ((sum 0))
    (save-excursion
      (goto-char p1)
      (save-match-data
        (while (re-search-forward sum-money-regexp p2 t)
          (setq sum (+ sum (sum-string-to-int (match-string 1)))))))
    (insert (format "[合計: %d]" sum))))

(defun sum-region (p1 p2)
  (interactive "r")
  (insert (format "[合計: %d]" (sum-string (buffer-substring p1 p2)))))

(defun sum-rectangle (p1 p2)
  (interactive "r")
  (let ((sum 0) (rect (extract-rectangle p1 p2)))
    (mapc (lambda (s) (setq sum (+ sum (sum-string s)))) rect)
    (insert (format "[合計: %d]" sum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Time and Date.

(defun make-timetable (&optional duration-minutes interval-minutes)
  (interactive)
  (let ((time-start nil) (time-end nil) hh mm)
    (setq duration-minutes
          (or duration-minutes
              (string-to-number (read-from-minibuffer "Frequency (minute): "))))
    (setq interval-minutes
          (or interval-minutes
              (string-to-number (read-from-minibuffer "Interval (minute): "))))

    (while (looking-at "\\([0-9]+\\):\\([0-9]+\\)\\( *- *[0-9]+:[0-9]+\\)?")
      (setq hh (string-to-number
                (buffer-substring (match-beginning 1) (match-end 1)))
            mm (string-to-number
                (buffer-substring (match-beginning 2) (match-end 2))))
      (delete-region (match-beginning 0) (match-end 0))

      (setq time-start
            (if time-start
                (+ time-start duration-minutes interval-minutes)
              (+ (* hh 60) mm)))
      (setq time-end (+ time-start duration-minutes))
      (save-excursion
        (insert (format "%02d:%02d-%02d:%02d"
                        (/ time-start 60) (% time-start 60)
                        (/ time-end 60) (% time-end 60))))
      (next-line 1))))

;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
