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

(defcustom mythic-chaos-factor 5
  "The chaos factor that influences fate checks.  Should be between 1 and 9.
See p. 19 of Mythic GME 2nd Edition."
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
  :type '(alist :key-type (string :tag "Label")
		:value-type (number :tag "Modifier")))

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
  :type '(alist :key-type (number :tag "Die result")
		:value-type (string :tag "Answer"))
  :group 'mythic)

(defcustom mythic-detail-check-modifiers '((3 . 2)
					   (6 . -2))
  "Modifiers to apply when the `mythic-chaos-factor' matches the CAR."
  :type '(alist :key-type (number :tag "Chaos Factor")
		:value-type (number :tag "Modifier"))
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

(defcustom mythic-event-focus-table '(((1  .  7)  . "Remote Event")
				      ((8  . 28)  . "NPC Action")
				      ((29 . 35)  . "Introduce a new NPC")
				      ((36 . 45)  . "Move toward a thread")
				      ((46 . 52)  . "Move Away from a thread")
				      ((53 . 55)  . "Close a thread")
				      ((56 . 67)  . "PC Negative")
				      ((68 . 75)  . "PC Positive")
				      ((76 . 83)  . "Ambiguous Event")
				      ((84 . 92)  . "NPC Negative")
				      ((93 . 100) . "NPC Positive"))
  "Event focus table, CAR is the range, CDR is the focus of the event.
See page 19 in Mythic Variations 2"
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
    (if (equal current-prefix-arg nil) (message result) (insert result))))


;; Mythic Variations 2 Detail Check:

(defun detail-check (question)
  "Interactive function for making a DETAIL CHECK for a particular QUESTION.
Run with a prefix argument to insert instead of echoing to the mini-buffer."
  (interactive "sDetail Check Question: \n")
  (let ((result "")
	(likelihood-modifier (car(assoc mythic-chaos-factor
				    mythic-detail-check-modifiers)))
	(dice-result))
    
    (if (eq likelihood-modifier nil)
	(setq likelihood-modifier 0))
    (setq dice-result (max
		       (min 18
			    (+ (roll-dice-total 2 10) likelihood-modifier))
		       4))
    (setq result (format "Q: %s\nA: %s"
			 question
			 (cdr (assoc dice-result mythic-detail-check-table))))

    (if (equal current-prefix-arg nil) (message result) (insert result))))

;; Extra meaning results

(defun mythic-description ()
  "Return a random selection from `mythic-meaning-table-descriptions'."
  (interactive)
  (let ((result))
    (setq result
	  (format "%s %s"
		  (seq-random-elt (car mythic-meaning-table-descriptions))
		  (seq-random-elt (cdr mythic-meaning-table-descriptions))))
    (if (equal current-prefix-arg nil) (message result) (insert result))))

(defun mythic-action ()
  "Return a random selection from `mythic-meaning-table-actions'."
  (interactive)
  (let ((result))
    (setq result
	  (format "%s %s"
		  (seq-random-elt (car mythic-meaning-table-actions))
		  (seq-random-elt (cdr mythic-meaning-table-actions))))
    (if (equal current-prefix-arg nil) (message result) (insert result))))


;; Mythic variations 2 chaos factor changes:

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


;; Behaviour checks:

(defcustom mythic-behaviour-check-disposition-table
  '(((5 . 5) . "Passive (-2)")
    ((6 . 10) . "Moderate (0)")
    ((11 . 15) . "Active (+2)")
    ((16 . 16) . "Aggressive (+4)"))
  "Disposition table for use with a `mythic-behaviour-check'.
See page 22 in Mythic Variations 2."
  :group 'mythic
  :type 'list)

(defcustom mythic-behaviour-check-npc-action-table-1
  '("Theme Action" "Theme Action" "Theme Action"
     "NPC Continues" "NPC Continues"
     "NPC Continues (+2)"
     "NPC Continues (-2)"
     "NPC Action"
     "NPC Action (-4)"
     "NPC Action (+4)")
  "NPC Action table 1.  See page 25 in Mythic Variations 2."
  :type 'list
  :group 'mythic)

(defcustom mythic-behaviour-check-npc-action-table-2
  '(((6 . 6) . "Talks, exposition")
    ((7 . 8) . "Performs an ambiguous action")
    ((9 . 10) . "Acts out of PC interest")
    ((11 . 11) . "Gives something")
    ((12 . 12) . "Seeks to end the encounter")
    ((13 . 13) . "Changes the theme")
    ((14 . 14) . "Changes descriptor")
    ((15 . 17) . "Acts out of self interest")
    ((18 . 18) . "Takes something")
    ((19 . 19) . "Causes harm"))
  "NPC Action table 2.  See page 26 in Mythic Variations 2."
  :type 'list
  :group 'mythic)

(defun mythic-behaviour-check--disposition-score (&optional n-descriptors-pos
							    n-descriptors-neg)
  "Determine the disposition score.
Using optional N-DESCRIPTORS-POS and N-DESCRIPTORS-NEG.
See page 22 of Mythic Variations 2."
  (let ((mod-add (* 2 (if (numberp n-descriptors-pos) n-descriptors-pos 0)))
	(mod-sub (* -2 (if (numberp n-descriptors-neg) n-descriptors-neg 0)))
	(mod-net) (disposition-score))
    (setq mod-net (+ mod-add mod-sub))
    (min 16 (max 5 (+ (roll-dice-total 2 10) mod-net)))
    ))

(defun mythic-behaviour-check--disposition (disposition-score)
  "Retrieve the matching disposition from the DISPOSITION-SCORE.
See `mythic-behaviour-check--disposition-score' for more details."
  (let ((result))
    (setq result (seq-filter #'(lambda (item)
				 (and (<= (car (car item)) disposition-score)
				      (>= (cdr (car item)) disposition-score)))
			     mythic-behaviour-check-disposition-table))
    (cdr (car result))))

(defun mythic-behaviour-check--action ()
  "Rolls on the NPC Action Table 1."
  (seq-random-elt mythic-behaviour-check-npc-action-table-1))

(defun mythic-behaviour-check--action-new (&optional disposition modifier)
  "Rolls on the NPC Action Table 2.
Applies optional DISPOSITION and MODIFIER to the roll.
See page 26 of Mythic Variations 2."
  (let ((roll (roll-dice-total 2 10))
	(net-mod (+ (if (numberp disposition) disposition 0)
		    (if (numberp modifier) modifier 0)))
	(result)
	(lookup))
    (setq lookup (min 19 (max (+ roll net-mod) 6)))
    (setq result (seq-filter #'(lambda (item)
				 (and (<= (car (car item)) lookup)
				      (>= (cdr (car item)) lookup)))
			     mythic-behaviour-check-npc-action-table-2))
    (cdr (car result))))


(defun mythic-behaviour-check-disposition (identity
					   personality activity
					   &optional n-desc-pos n-desc-neg)
  "Interactive function for checking the disposition.
Takes inputs for IDENTITY, PERSONALITY, and ACTIVITY for user reference.
Takes inputs for the N-DESC-POS and N-DESC-NEG for calculating disposition.
If called with a prefix then insert the result at point instead of returning a
message.
See page 21 of Mythic Variations 2."
  (interactive "sIdentity: \nsPersonality: \nsActivity: \nnActive Positive Descriptors: \nnActive Negative Descriptors: ")

  (let ((result))
    (setq result
	  (format
	   "Identity: %s\nPersonality: %s\nActivity: %s\nDispostition: %s"
	   identity personality activity
	   (mythic-behaviour-check--disposition
	    (mythic-behaviour-check--disposition-score
	     n-desc-pos n-desc-neg))))
    (if (equal current-prefix-arg nil) (message result) (insert result))))


(defun mythic-behaviour-check-action (theme actor)
  "Make a behaviour check for the THEME and ACTOR.
If called with a prefix then insert the result at current point instead of
message."
  (interactive "sTheme: \nsActor: ")
  (let ((action (mythic-behaviour-check--action))
	(result))
    (setq result (format "Theme: %s\nActor: %s\nAction: %s"
			 theme actor action))
    (if (equal current-prefix-arg nil) (message result) (insert result))))


(defun mythic-behaviour-check-new-action (theme actor disposition modifier
						&optional insert)
  "Make a new action check for the THEME and ACTOR.
DISPOSITION and MODIFIER are passed to `mythic-behaviour-check--action-new'.
If INSERT is t then insert at point instead of echoing to minibuffer."
  (interactive
   "sTheme: \nsActor: \nsCurrent Disposition: \nsAdditional Modifier: ")
  (let ((action (mythic-behaviour-check--action-new))
	(result))
    (setq result (format "Theme: %s\nActor: %s\nAction: %s"
			 theme actor action))
    (if insert (insert result) (message result))))

(defun mythic-behaviour-check-new-action-insert (theme actor
						       disposition modifier)
  "Wraps `mythic-behaviour-check-action' with INSERT as t.
THEME, ACTOR, DISPOSITION, and MODIFIER are passed to the parent function."
  (interactive
   "sTheme: \nsActor: \nsCurrent Disposition: \nsAdditional Modifier: ")
  (mythic-behaviour-check-new-action theme actor disposition modifier t))

;; Statistics checks:

(defcustom mythic-statistic-check-modifiers
  '(("Important NPC" . 2)
    ("Weak Attribute" . -2)
    ("Strong Attribute" . 2)
    ("Prime Attribute" . 4))
  "Statsitcs Check Modifiers.  See p. 37 of Mythic Variations 2."
  :type 'list
  :group 'mythic)

(defcustom mythic-statistic-check-table
  '(((0  .  2) . "Very weak (-75%)")
    ((3  .  4) . "Weak (-50%)")
    ((5  .  6) . "Less (-10%)")
    ((7  . 11) . "Expected Baseline")
    ((12 . 14) . "More (+10%)")
    ((15 . 16) . "Strong (+50%)")
    ((17 . 18) . "Very Strong (+100%)")
    ((19 . 20) . "PC Baseline")
    ((21 . 22) . "PC Mode (+10%)")
    ((23 . 24) . "PC Strong (+50%)")
    ((25 . 26) . "PC Very Strong (+100%)"))
  "Statisitc Check Table.  See page 37 in Mythic Variations 2."
  :type 'list
  :group 'mythic)

(defun mythic-statistic--check (npc-modifier)
  "Rolls on the `mythic-statistic-check-table' with the NPC-MODIFIER."
  (let ((dice-roll (roll-dice-total 2 10))
	(lookup)
	(result))

    (setq lookup (+ npc-modifier dice-roll))
    (setq result (seq-filter #'(lambda (item)
				 (and (<= (car (car item)) lookup)
				      (>= (cdr (car item)) lookup)))
			     mythic-statistic-check-table))
    (cdr (car result))))

(defun mythic-statistic-check (actor attribute &optional npc-modifier insert)
  "Interactive function to make a statistics check for ACTOR's ATTRIBUTE.
NPC-MODIFIER is the net modifier from `mythic-statistic-check-modifiers'.
If INSERT is t then insert at current point instead of to minibuffer."
  (interactive "sActor: \nsAttribute: \nn Net modifiers: ")
  (let ((result))
    (setq result
	  (format "%s's %s is %s"
		  actor attribute (mythic-statistic--check
				  (if (numberp npc-modifier) npc-modifier 0))))
    (if insert (insert result) (message result))))
    
(defun mythic-statistic-check-insert (actor attribute &optional npc-modifier)
  "Wraps `mythic-statistic-check' with INSERT as t.
ACTOR, ATTRIBUTE, and NPC-MODIFIER are passed to the parent function."
  (interactive "sActor: \nsAttribute: \nn Net modifiers: ")
  (mythic-statistic-check actor attribute npc-modifier t))


;; Success rolls:

(defun ttrpg-roll-under-p (target n-dice n-sides)
  "Check if a dice roll with N-DICE and N-SIDES was under or eq to a TARGET."
  (<= (roll-dice-total n-dice n-sides)))


;; UNE:

;;; TODO: put UNE mechanics here
  
(provide 'ttrpg)
;;; ttrpg.el ends here
