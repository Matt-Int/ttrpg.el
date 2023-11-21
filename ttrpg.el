;; ttrpg.el --- Roll dice and use Mythic GM Emulator functionality
;; Author: Mattias <mattias1126@protonmail.com>
;; Maintainer: Mattias <mattias1126@protonmail.com>
;; Keywords: ttrpg games
;; Version 0.1.0
;; URL: <todo: include url>
;; Package-Requires: ((emacs "29.0.50"))

;;; Commentary:

;; Provides a set of helper functions for running solo-ttrpg games

;;; Code:

;; Config and customisations:

(defgroup ttrpg nil
  "Running solo-ttrpgs."
  :group 'games)

(defgroup mythic nil
  "Mythic GM Emulator (2nd Edition)."
  :group 'ttrpg)

(defcustom mythic-chaos-factor 5
  "The chaos factor that influences fate checks.  Should be between 1 and 9.
See p. 19, 22 of Mythic GME 2nd Edition."
  :group 'mythic
  :type 'integer)

(make-variable-buffer-local 'mythic-chaos-factor)

(defcustom mythic-fate-check-modifiers '(("50/50"              .  0)
					 ("Likely"             .  1)
					 ("Unlikely"           . -1)
					 ("Very Likely"        .  2)
					 ("Very Unlikely"      . -2)
					 ("Nearly Certain"     .  4)
					 ("Nearly Impossible"  . -4)
					 ("Certain"            .  5)
					 ("Impossible"         . -5))
  "Sets the modifiers for the various likelihoods.
See p.26 of Mythic GME 2nd Edition."
  :group 'mythic
  :type '(alist :key-type (string :tag "Label")
		:value-type (number :tag "Modifier")))

(defcustom mythic-meaning-tables '((("Result 1a" "Result 2a")
				    ("Result 3a" "Result 4a"))
				   (("Result 1d" "Result 2d")
				    ("Result 3d" "Result 4d")))
  "Meaning Table: Descriptions & Actions from Mythic 2nd Edition.
See page 46-47 of Mythic 2nd Edition.
Value is a nested list in the order of Action table 1 entries,
Action table 2 entries,
Description table 1 entrie, and description table 2 entries."
  :type '(list :tag "Mythic Meaning Tables"
	  (list :tag "Actions" (repeat :tag "Table 1" (string :tag "Action"))
		(repeat :tag "Table 2" (string :tag "Action")))
	  (list :tag "Descriptions" (repeat :tag "Table 1" (string :tag "Description"))
		(repeat :tag "Table 2" (string :tag "Description"))))
  :group 'mythic)

(defcustom mythic-meaning-tables-elements '(("Table Name"
					     "Entry 1" "Entry 2"))
  "Meaning Table: Elements from Mythic 2nd Edition, see pages 201-215."
  :group 'mythic
  :type '(alist :key-type (string :tag "Element Table")
		:value-type (repeat (string :tag "Element"))))

(defcustom mythic-event-focus-table '(((1  .  5)  . "Remote Event")
				      ((6  . 10)  . "Ambiguous Event")
				      ((11 . 20)  . "New NPC")
				      ((21 . 40)  . "NPC Action")
				      ((41 . 45)  . "NPC Negative")
				      ((46 . 50)  . "NPC Positive")
				      ((51 . 55)  . "move towards a thread")
				      ((56 . 65)  . "Move Away From A Thread")
				      ((66 . 70)  . "Close A Thread")
				      ((71 . 80)  . "PC Negative")
				      ((81 . 85)  . "PC Positive")
				      ((86 . 100) . "Current Context"))
  "Event focus table, CAR is the range, CDR is the focus of the event.
See page 37 in Mythic 2nd Edition"
  :group 'mythic
  :type '(alist :key-type (alist :key-type (number :tag "min")
				 :value-type (number :tag "max"))
		:value-type (string :tag "Event")))

(defun mythic-fate-check-modifier-options ()
  "Gets the options from `mythic-fate-check-modifiers'.
These options are available to select when doing a FATE CHECK."
  (mapc #'(lambda (item) (car item)) mythic-fate-check-modifiers))

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

;; Mythic 2nd Edition Fate Check:

(defun between-p (number low high)
  "Compares NUMBER if it is between LOW and HIGH.
Comparison is inclusive on both ends."
  (and (<= number high) (>= number low)))

(defun calc-chaos-modifier (chaos-rank)
  "Calculates what the modification is based on the CHAOS-RANK."
  (round (+ -6.167 (* 1.233 chaos-rank)))) ; calculated using linear regression, go stats!


(defun chaos-roll (chaos-rank likelihood-modifier)
  "Rolls 2d10 and sums them up.
The total +- the LIKELIHOOD-MODIFIER and CHAOS-RANK is checked against 11.
If the result is between 18-20 it is an Exceptional Yes.
If the result is between 2-4 it is an Exceptional No.
If both dice are the same and within the Chaos factor then a random
event occurs.
See p.25-26 of Mythic 2nd Edition."

  (let ((die-one (roll-die 10))
        (die-two (roll-die 10))
	(roll-modifier (+ likelihood-modifier (calc-chaos-modifier chaos-rank)))
        (response "No"))
    (if (>= (+ die-one die-two) (+ 11 roll-modifier))
        (setq response "Yes"))
    (if (or (between-p (+ die-one die-two roll-modifier) 18 20)
	    (between-p (+ die-one die-two roll-modifier)  2  4))
	(setq response (concat response " + Exceptional")))
    (if (and (eq die-one die-two) (<= die-one chaos-rank))
        (setq response (concat response " + Random Event")))
    (message "d1: %s d2: %s modifier: %s total: %s result: %s"
	     die-one die-two roll-modifier (+ die-one die-two roll-modifier) response)
    response))

(defun fate-check (question)
  "Interactive function for making a FATE CHECK for a particular QUESTION.
Run with a prefix argument to insert at point instead of echoing to the
mini-buffer."
  (interactive "sFate Check Question: \n")
  (let ((likelihood (completing-read "How likely is this? "
				     (mythic-fate-check-modifier-options)))
	(result "")
	(likelihood-modifier))
    (setq likelihood-modifier (cdr (assoc likelihood
					  mythic-fate-check-modifiers)))
    (setq result
	  (format "Q: %s (%s)\nA: %s"
		  question
		  likelihood
		  (chaos-roll
		   mythic-chaos-factor
		   likelihood-modifier)))
    (if (equal current-prefix-arg nil) (message result) (insert result))))


;; Extra meaning results

(defun mythic-description ()
  "Return a random selection from the description part of `mythic-meaning-tables'."
  (interactive)
  (let ((result))
    (setq result
	  (format "%s %s"
		  (seq-random-elt (car (car (cdr mythic-meaning-tables))))
		  (seq-random-elt (car (cdr (car (cdr mythic-meaning-tables)))))))
    (if (equal current-prefix-arg nil) (message result) (insert result))))

(defun mythic-action ()
  "Return a random selection from the action part of `mythic-meaning-tables'."
  (interactive)
  (let ((result))
    (setq result
	  (format "%s %s"
		  (seq-random-elt (car (car mythic-meaning-tables)))
		  (seq-random-elt (car (cdr (car mythic-meaning-tables))))))
    (if (equal current-prefix-arg nil) (message result) (insert result))))

(defun mythic-elements (element)
  "Find 2 ELEMENT's from `mythic-meaning-tables-elements'."
  (let ((result))
    (setq result
	  (format "%s %s"
		  (seq-random-elt
		   (cdr (car (seq-filter #'(lambda (item) (equal (car item) element)) mythic-meaning-tables-elements))))
		  (seq-random-elt
		   (cdr (car (seq-filter #'(lambda (item) (equal (car item) element)) mythic-meaning-tables-elements))))))))


;; Mythic 2nd Edition chaos factor changes:

(defun chaos-factor-increase ()
  "Increases the chaos factor by one unless it is at the maximum.
It also resets it to the max in case it has somehow gone beyond the max."
  (if (> mythic-chaos-factor 8)
      (progn
	(message "Chaos factor is already at max.")
	(setq mythic-chaos-factor 9))
    (setq mythic-chaos-factor (+ 1 mythic-chaos-factor))))

(defun chaos-factor-decrease ()
  "Decreases the chaos factor by one unless it is at the minimum.
It also resets to the minimum in case it has somehow gone beyond the min."
  (if (< mythic-chaos-factor 2)
      (progn
	(message "Chaos factor is already at min.")
	(setq mythic-chaos-factor 1))
    (setq mythic-chaos-factor (- mythic-chaos-factor 1))))


;; Event Checks:

(defun event--check ()
  "Make an EVENT-CHECK.  See page 18 in Mythic Variations 2."
  (let ((dice-roll (roll-dice-total 1 100)))
    (seq-filter #'(lambda (item)
		    (and (>= (cdr (car item)) dice-roll)
			 (<= (car (car item)) dice-roll)))
		mythic-event-focus-table)))

(defun event-check ()
  "Interactive function for making an EVENT CHECK.
Call with a prefix to insert at point instead of echoing to the
mini-buffer."
  (interactive)
  (let ((result ""))
    (setq result (format "Event Focus: *%s*"
			 (cdr (car (event--check)))))

    (if (equal current-prefix-arg nil) (message result) (insert result))))

;; Statistics checks:

(defun mythic-statistic--check (expected)
  "Roll a fate question and modify EXPECTED stats accordingly.
See p.127 in Mythic 2nd Edition"
  (let ((answer (chaos-roll mythic-chaos-factor 0))
	(result))
    (cond
     ((and (string-match-p "Yes" answer)
	   (not (string-match-p "Exceptional" answer))) (setq result expected))
     ((and (string-match-p "No" answer)
	   (not (string-match-p "Exceptional" answer))) (setq expected (* expected 0.75)))
     ((string-match-p "\\(Yes\\)\\(.*Exceptional\\)\\(.*Random Event\\)?" answer) (setq expected (* expected 1.25)))
     ((string-match-p "\\(No\\)\\(.*Exceptional\\)\\(.*Random Event\\)?" answer) (setq expected (* expected 0.5))))
    (if (string-match-p "Random Event" answer)
	(setq result (format "%s (Special Condition)" expected))
      (setq result (format "%s" expected)))
    result))

(defun mythic-statistic-check (actor attribute expected &optional insert)
  "Interactive function to make a statistics check for ACTOR's ATTRIBUTE.
Depending on results it will be what is EXPECTED, stronger or weaker.
If INSERT is t then insert at current point instead of to minibuffer."
  (interactive "sActor: \nsAttribute: \nnExpected: ")
  (let ((result))
    (setq result
	  (format "%s's %s is %s"
		  actor attribute (mythic-statistic--check expected)))
    (if insert (insert result) (message result))))
    
(defun mythic-statistic-check-insert (actor attribute expected)
  "Wraps `mythic-statistic-check' with INSERT as t.
ACTOR, ATTRIBUTE, and EXPECTED are passed to the parent function."
  (interactive "sActor: \nsAttribute: \nn Net modifiers: ")
  (mythic-statistic-check actor attribute expected t))


;; Success rolls:

(defun ttrpg-roll-under-p (target n-dice n-sides)
  "Check if a dice roll with N-DICE and N-SIDES was under or equal to a TARGET."
  (<= (roll-dice-total n-dice n-sides)))

(defun ttrpg-roll-over-p (target n-dice n-sides)
  "Check if a dice roll with N-DICE and N-SIDES was over or equal to a TARGET."
  (>= (roll-dice-total n-dice n-sides)))

;; UNE:

;;; TODO: put UNE mechanics here
  
(provide 'ttrpg)
;;; ttrpg.el ends here
