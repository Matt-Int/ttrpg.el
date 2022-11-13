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
  "Mythic GM Emulator (variation 2)."
  :group 'ttrpg)

(defcustom mythic-chaos-factor 4
  "The chaos factor that influences fate checks.  Should be between 3 and 6.
See p. 6 of Mythic Variations 2."
  :group 'mythic
  :type 'integer)

(make-variable-buffer-local 'mythic-chaos-factor)

(defcustom mythic-fate-check-modifiers '(("50/50 or Unsure" .  0)
					 ("Likely"          .  2)
					 ("Unlikely"        . -2)
					 ("Very Likely"     .  4)
					 ("Very Unlikely"   . -4)
					 ("Sure Thing"      .  6)
					 ("No Way"          . -6)
					 ("Has to be"       .  8)
					 ("Impossible"      . -8))
  "Sets the modifiers for the various likelihoods."
  :group 'mythic
  :type 'list)

(defcustom mythic-detail-check-table '((4 . "Anger")
				       (5 . "Sadness")
				       (6 . "Fear")
				       (7 . "Disfavours Thread")
				       (8 . "Disfavours NPC")
				       (9 . "Focus NPC")
				       (10 . "Favours NPC")
				       (11 . "Focus PC")
				       (12 . "Disfavours NPC")
				       (13 . "Focus Thread")
				       (14 . "Favours PC")
				       (15 . "Favours Thread")
				       (16 . "Courage")
				       (17 . "Happiness")
				       (18 . "Calm"))
  "A DETAIL CHECK table.  See page 11 of Mythic Variations 2."
  :type 'list
  :group 'mythic)

(defcustom mythic-detail-check-modifiers '((3 . 2)
					   (6 . -2))
  "Modifiers to apply when the `mythic-chaos-factor' matches the CAR."
  :type 'list
  :group 'mythic)

(defcustom mythic-meaning-table-descriptions
  (cons '("Result: 1"  "Result: 2" "Result: 3" "Result: 5"
	  "Result: 6" "Result: 7" "Result: 8" "Result: 9"
	  "Result: 10" "Result: 11" "Result: 12" "Result: 13"
	  "Result: 14" "Result: 15" "Result: 16" "Result: 17"
	  "Result: 18" "Result: 19" "Result: 20" "Result: 21"
	  "Result: 22" "Result: 23" "Result: 24" "Result: 25"
	  "Result: 26" "Result: 27" "Result: 28" "Result: 29"
	  "Result: 30" "Result: 31" "Result: 32" "Result: 33"
	  "Result: 34" "Result: 35" "Result: 36" "Result: 37"
	  "Result: 38" "Result: 39" "Result: 40" "Result: 41"
	  "Result: 42" "Result: 43" "Result: 44" "Result: 45"
	  "Result: 46" "Result: 47" "Result: 48" "Result: 49"
	  "Result: 50" "Result: 51" "Result: 52" "Result: 53"
	  "Result: 54" "Result: 55" "Result: 56" "Result: 57"
	  "Result: 58" "Result: 59" "Result: 60" "Result: 61"
	  "Result: 62" "Result: 63" "Result: 64" "Result: 65"
	  "Result: 66" "Result: 67" "Result: 68" "Result: 69"
	  "Result: 70" "Result: 71" "Result: 72" "Result: 73"
	  "Result: 74" "Result: 75" "Result: 76" "Result: 77"
	  "Result: 78" "Result: 79" "Result: 80" "Result: 81"
	  "Result: 82" "Result: 83" "Result: 84" "Result: 85"
	  "Result: 86" "Result: 87" "Result: 88" "Result: 89"
	  "Result: 90" "Result: 91" "Result: 92" "Result: 93"
	  "Result: 94" "Result: 95" "Result: 96" "Result: 97"
	  "Result: 98" "Result: 99" "Result: 100")
	'("Result: 1"  "Result: 2" "Result: 3" "Result: 5"
	  "Result: 6" "Result: 7" "Result: 8" "Result: 9"
	  "Result: 10" "Result: 11" "Result: 12" "Result: 13"
	  "Result: 14" "Result: 15" "Result: 16" "Result: 17"
	  "Result: 18" "Result: 19" "Result: 20" "Result: 21"
	  "Result: 22" "Result: 23" "Result: 24" "Result: 25"
	  "Result: 26" "Result: 27" "Result: 28" "Result: 29"
	  "Result: 30" "Result: 31" "Result: 32" "Result: 33"
	  "Result: 34" "Result: 35" "Result: 36" "Result: 37"
	  "Result: 38" "Result: 39" "Result: 40" "Result: 41"
	  "Result: 42" "Result: 43" "Result: 44" "Result: 45"
	  "Result: 46" "Result: 47" "Result: 48" "Result: 49"
	  "Result: 50" "Result: 51" "Result: 52" "Result: 53"
	  "Result: 54" "Result: 55" "Result: 56" "Result: 57"
	  "Result: 58" "Result: 59" "Result: 60" "Result: 61"
	  "Result: 62" "Result: 63" "Result: 64" "Result: 65"
	  "Result: 66" "Result: 67" "Result: 68" "Result: 69"
	  "Result: 70" "Result: 71" "Result: 72" "Result: 73"
	  "Result: 74" "Result: 75" "Result: 76" "Result: 77"
	  "Result: 78" "Result: 79" "Result: 80" "Result: 81"
	  "Result: 82" "Result: 83" "Result: 84" "Result: 85"
	  "Result: 86" "Result: 87" "Result: 88" "Result: 89"
	  "Result: 90" "Result: 91" "Result: 92" "Result: 93"
	  "Result: 94" "Result: 95" "Result: 96" "Result: 97"
	  "Result: 98" "Result: 99" "Result: 100"))
  "Meaning Table: Descriptions from Mythic Variations 2, see page 16.
Value should be a cons of two lists with the CAR matching description table
1 and the CDR matching description table 2."
  :type 'list
  :group 'mythic)

(defcustom mythic-meaning-table-actions
  (cons '("Result: 1"  "Result: 2" "Result: 3" "Result: 5"
	  "Result: 6" "Result: 7" "Result: 8" "Result: 9"
	  "Result: 10" "Result: 11" "Result: 12" "Result: 13"
	  "Result: 14" "Result: 15" "Result: 16" "Result: 17"
	  "Result: 18" "Result: 19" "Result: 20" "Result: 21"
	  "Result: 22" "Result: 23" "Result: 24" "Result: 25"
	  "Result: 26" "Result: 27" "Result: 28" "Result: 29"
	  "Result: 30" "Result: 31" "Result: 32" "Result: 33"
	  "Result: 34" "Result: 35" "Result: 36" "Result: 37"
	  "Result: 38" "Result: 39" "Result: 40" "Result: 41"
	  "Result: 42" "Result: 43" "Result: 44" "Result: 45"
	  "Result: 46" "Result: 47" "Result: 48" "Result: 49"
	  "Result: 50" "Result: 51" "Result: 52" "Result: 53"
	  "Result: 54" "Result: 55" "Result: 56" "Result: 57"
	  "Result: 58" "Result: 59" "Result: 60" "Result: 61"
	  "Result: 62" "Result: 63" "Result: 64" "Result: 65"
	  "Result: 66" "Result: 67" "Result: 68" "Result: 69"
	  "Result: 70" "Result: 71" "Result: 72" "Result: 73"
	  "Result: 74" "Result: 75" "Result: 76" "Result: 77"
	  "Result: 78" "Result: 79" "Result: 80" "Result: 81"
	  "Result: 82" "Result: 83" "Result: 84" "Result: 85"
	  "Result: 86" "Result: 87" "Result: 88" "Result: 89"
	  "Result: 90" "Result: 91" "Result: 92" "Result: 93"
	  "Result: 94" "Result: 95" "Result: 96" "Result: 97"
	  "Result: 98" "Result: 99" "Result: 100")
	'("Result: 1"  "Result: 2" "Result: 3" "Result: 5"
	  "Result: 6" "Result: 7" "Result: 8" "Result: 9"
	  "Result: 10" "Result: 11" "Result: 12" "Result: 13"
	  "Result: 14" "Result: 15" "Result: 16" "Result: 17"
	  "Result: 18" "Result: 19" "Result: 20" "Result: 21"
	  "Result: 22" "Result: 23" "Result: 24" "Result: 25"
	  "Result: 26" "Result: 27" "Result: 28" "Result: 29"
	  "Result: 30" "Result: 31" "Result: 32" "Result: 33"
	  "Result: 34" "Result: 35" "Result: 36" "Result: 37"
	  "Result: 38" "Result: 39" "Result: 40" "Result: 41"
	  "Result: 42" "Result: 43" "Result: 44" "Result: 45"
	  "Result: 46" "Result: 47" "Result: 48" "Result: 49"
	  "Result: 50" "Result: 51" "Result: 52" "Result: 53"
	  "Result: 54" "Result: 55" "Result: 56" "Result: 57"
	  "Result: 58" "Result: 59" "Result: 60" "Result: 61"
	  "Result: 62" "Result: 63" "Result: 64" "Result: 65"
	  "Result: 66" "Result: 67" "Result: 68" "Result: 69"
	  "Result: 70" "Result: 71" "Result: 72" "Result: 73"
	  "Result: 74" "Result: 75" "Result: 76" "Result: 77"
	  "Result: 78" "Result: 79" "Result: 80" "Result: 81"
	  "Result: 82" "Result: 83" "Result: 84" "Result: 85"
	  "Result: 86" "Result: 87" "Result: 88" "Result: 89"
	  "Result: 90" "Result: 91" "Result: 92" "Result: 93"
	  "Result: 94" "Result: 95" "Result: 96" "Result: 97"
	  "Result: 98" "Result: 99" "Result: 100"))
  "Meaning Table: Actions from Mythic Variations 2, see page 17.
Value should be a cons of two lists with the CAR matching action table
1 and the CDR matching action table 2."
  :type 'list
  :group 'mythic)

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

;; Mythic Variations 2 Fate Check:

(defun chaos-roll (chaos-rank likelihood-modifier)
  "Rolls 3d10, where one die is the chaos die.
The total is checked against some threshold +- the LIKELIHOOD-MODIFIER.
The chaos die is checked against the CHAOS-RANK to see if a random
event occurs.
See p.6 of Mythic Variations 2."

  (let ((chaos-die (roll-die 10))
        (die-one (roll-die 10))
        (die-two (roll-die 10))
        (response "No!"))
    (if (< (+ die-one die-two) (+ 11 likelihood-modifier))
        (setq response "Yes!"))
    (if (and (eq die-one die-two) (< chaos-die chaos-rank))
        (setq response (concat response " (Extreme + Random Event)"))
      (if (and (eq 0 (mod die-one 2)) (eq 0 (mod die-two 2)))
          (setq response (concat response " (Random Event)"))
        (if (and (not (eq (mod die-one 2) 0)) (not (eq (mod die-two 2) 0)))
            (setq response (concat response " (Extreme)")))))
    response))

(defun fate-check (question &optional insert)
  "Interactive function for making a FATE CHECK for a particular QUESTION.
Set the optional INSERT to t to insert at point instead of echoing to the
mini-buffer."
  (interactive "sFate Check Question: \n")
  (let ((likelihood (completing-read "How likely is this? "
				     (mythic-fate-check-modifier-options)))
	(result "")
	(likelihood-modifier))
    (setq likelihood-modifier (cdr (assoc likelihood
					  mythic-fate-check-modifiers)))
    (if (eq 3 mythic-chaos-factor)
	(if (yes-or-no-p "Is a yes favourable?")
	    (setq likelihood-modifier (+ 2 likelihood-modifier))
	  (setq likelihood-modifier (- 2 likelihood-modifier)))
      (if (eq 6 mythic-chaos-factor)
	  (if (yes-or-no-p "Is a yes favourable?")
	      (setq likelihood-modifier (- 2 likelihood-modifier))
	    (setq likelihood-modifier (+ 2 likelihood-modifier)))))
    (setq result
	  (format "Q: %s (%s)\nA: %s"
		  question
		  likelihood
		  (chaos-roll
		   mythic-chaos-factor
		   likelihood-modifier)))
    (if (eq insert t) (insert result) (message result))))

(defun fate-check-insert (question)
  "Interactive function to insert the result of a FATE CHECK for a QUESTION.
See `fate-check' for more details"
  (interactive "sFate Check Question: \n")
  (fate-check question t))


;; Mythic variations 2 chaos factor changes:

(defun chaos-factor-increase ()
  "Increases the chaos factor by one unless it is at the maximum.
It also resets it to the max in case it has somehow gone beyond the max."
  (if (> mythic-chaos-factor 5)
      (progn
	(message "Chaos factor is already at max.")
	(setq mythic-chaos-factor 6))
    (setq mythic-chaos-factor (+ 1 mythic-chaos-factor))))

(defun chaos-factor-decrease ()
  "Decreases the chaos factor by one unless it is at the minimum.
It also resets to the minimum in case it has somehow gone beyond the min."
  (if (< mythic-chaos-factor 4)
      (progn
	(message "Chaos factor is already at min.")
	(setq mythic-chaos-factor 3))
    (setq mythic-chaos-factor (- mythic-chaos-factor 1))))

(provide 'ttrpg)
;;; ttrpg.el ends here
