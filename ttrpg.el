;; ttrpg.el --- Roll dice and other utility functions for general ttrpg-ing.
;; Author: Mattias <mattias1126@protonmail.com>
;; Maintainer: Mattias <mattias1126@protonmail.com>
;; Keywords: ttrpg games
;; Version 0.1.0
;; URL: <todo: include url>
;; Package-Requires: ((emacs "29.0.50") (cl-lib))

;;; Commentary:

;; Provides a set of helper functions for running solo-ttrpg games

;;; Code:

(require 'cl-lib)

;; Config and customisations:

(defgroup ttrpg nil
  "Running solo-ttrpgs."
  :group 'games)

  
;; Util functions for rolling dice:

(defun roll-die (n-sides)
  "Rolls a single die with N-SIDES."
  (+ 1 (random n-sides)))

(defun roll-dice (n-dice n-sides)
  "Rolls N-DICE with N-SIDES each."
  (let ((rolls '()))
    (dotimes (number n-dice)
      (push (roll-die n-sides) rolls))
    rolls))

(defun roll-dice-total (n-dice n-sides)
  "Rolls N-DICE with N-SIDES and sums the result."
  (seq-reduce #'+ (roll-dice n-dice n-sides) 0))

;; Dice notation parser

(defun parse-dice-notation--simple (notation)
  "Parse a standard AdY NOTATION and use `roll-dice-total'."
  (let ((sides (mapcar #'string-to-number (string-split notation "d"))))
    (roll-dice-total (car sides) (car (cdr sides)))))

(defun parse-dice-notation (notation)
  "Parse standard AdY + C NOTATION and call appropriate functions."
  (save-match-data
    (let ((result notation))
      (while (string-match "\\([0-9]+d[0-9]+\\)" result)
	(let ((match (match-string 1 result)))
	  (setq result (replace-regexp-in-string match (number-to-string
							 (parse-dice-notation--simple match))
						 result nil nil nil)))
	)
      (string-to-number (calc-eval result)))))


(parse-dice-notation "2d20 + 1d10")

(defun dice-roll (notation)
  "Evaluate a dice NOTATION."
  (interactive "sDice Notation: ")
  (let ((result (parse-dice-notation notation)))
    (if current-prefix-arg
	(insert (format "%s → %d" notation result)))
    (message "%s → %d" notation result)
    ))

;; Util functions for reading random entries from a file

(defun read-random-line-file (filename)
  "Read a random line from FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((lines (car (page--count-lines-page))))
      (forward-line (random (- lines 1)))
      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(defun between-p (number low high)
  "Compares NUMBER if it is between LOW and HIGH.
Comparison is inclusive on both ends."
  (and (<= number high) (>= number low)))

;; Success rolls:

(defun ttrpg-roll-under-p (target n-dice n-sides)
  "Check if a dice roll with N-DICE and N-SIDES was under or equal to a TARGET."
  (<= (roll-dice-total n-dice n-sides)))

(defun ttrpg-roll-over-p (target n-dice n-sides)
  "Check if a dice roll with N-DICE and N-SIDES was over or equal to a TARGET."
  (>= (roll-dice-total n-dice n-sides)))

(provide 'ttrpg)
;;; ttrpg.el ends here
