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

(defgroup ttrpg nil
  "Running solo-ttrpgs."
  :group 'games)

(defcustom mythic-chaos-factor 4
  "The chaos factor that influences fate checks.  Should be between 3 and 6.
See p. 6 of Mythic Variations 2."
  :group 'ttrpg
  :type 'integer)

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
  :group 'ttrpg
  :type 'list)

(defun mythic-fate-check-modifier-options ()
  "Gets the options from `mythic-fate-check-modifiers'.
These options are available to select when doing a FATE CHECK."
  (mapc #'(lambda (item) (car item)) mythic-fate-check-modifiers))

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
    (message response)))

(defun fate-check (question)
  "Interactive function for making a FATE CHECK for a particular QUESTION."
  (interactive "sFate Check Question: \n")
  (message (format "Q: %s \nA: %s" question (chaos-roll
					     mythic-chaos-factor
					     0))))
(provide 'ttrpg)
;;; ttrpg.el ends here
