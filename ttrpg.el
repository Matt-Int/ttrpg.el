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
  "Meaning Table: Descriptions from Mythic 2nd Edition, see page 47.
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
  "Meaning Table: Actions from Mythic 2nd Edition, see page 48.
Value should be a cons of two lists with the CAR matching action table
1 and the CDR matching action table 2."
  :type 'list
  :group 'mythic)

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
