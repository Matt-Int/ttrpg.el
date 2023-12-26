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
  "This is based on the Mythic Game Master Emulator Second Edition by Tana Pigeon.
It is published by Word Mill Games, and licensed for our use under the Creative
Commons Attribution-NonCommercial 4.0 International (CC BY-NC 4.0) license.
Find out more at www.wordmillgames.com"
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

(defcustom mythic-event-focus-table '(((1  5)  .  "Remote Event")
				      ((6  10)  . "Ambiguous Event")
				      ((11 20)  . "New NPC")
				      ((21 40)  . "NPC Action")
				      ((41 45)  . "NPC Negative")
				      ((46 50)  . "NPC Positive")
				      ((51 55)  . "move towards a thread")
				      ((56 65)  . "Move Away From A Thread")
				      ((66 70)  . "Close A Thread")
				      ((71 80)  . "PC Negative")
				      ((81 85)  . "PC Positive")
				      ((86 100) . "Current Context"))
  "Event focus table, CAR is the range, CDR is the focus of the event.
See page 37 in Mythic 2nd Edition"
  :group 'mythic
  :type '(alist :key-type (list (number :tag "min")
				(number :tag "max"))
		:value-type (string :tag "Event")))

(defun mythic-fate-check-modifier-options ()
  "Gets the options from `mythic-fate-check-modifiers'.
These options are available to select when doing a FATE CHECK."
  (mapc #'(lambda (item) (car item)) mythic-fate-check-modifiers))

;; Mythic Adventures

(defgroup mythic-adventure nil
  "Options related to Adventures run using Mythic GME."
  :group 'mythic)

(defcustom mythic-adventure-current nil
  "The ID of the current adventure."
  :group 'mythic-adventure
  :type 'string)

(defcustom mythic-adventure-directory (expand-file-name "var/mythic/adventures" user-emacs-directory)
  "The directory where adventures will be stored."
  :type 'directory)

(defun mythic-adventure-new (name)
  "Create a new adventure with with a specified NAME."
  (interactive "sName of Adventure: ")
  (let ((adventure-dir (format "%s/%s" mythic-adventure-directory name))
	(default-files '("adventure.log" "lists-npcs.org" "lists-threads.org" "scenes.org")))
    (if (make-directory adventure-dir t)
	(message "An adventure with that name already exists.")
      (setq mythic-adventure-current name)
      (mapc #'(lambda (file)
		(write-region "" nil (format "%s/%s" adventure-dir file)))
	    default-files)
      (mythic-log "Creating new adventure: '%s'" name)
    )))

(defun mythic-adventure--list-choices ()
  "List all of the current adventures in `mythic-adventure-directory'."
  (cdr (cdr (directory-files mythic-adventure-directory))))

(defun mythic-adventure-pick ()
    "Pick an adventure and make it the active one."
  (interactive)
  (let ((adventure (completing-read "Adventure: " (mythic-adventure--list-choices))))
    (setq mythic-adventure-current nil)
    (mythic-log "Changing adventure to: '%s'" adventure)
    (setq mythic-adventure-current adventure)))

(defun mythic-adventure--list (file)
  "Get the headers from the FILE list.
File should be either `npcs' or `threads'."
  (let ((file (format "%s/%s/lists-%s.org"
			  mythic-adventure-directory
			  mythic-adventure-current
			  file)))
    (with-temp-buffer
      (goto-char (point-min))
      (insert-file-contents file)
      (mapcan #'(lambda (item) (split-string item "*" t " "))
	      (seq-filter #'(lambda (line) (string-match-p "^\\*" line))
			  (split-string (buffer-string) "\n" t))))))

(defun mythic-adventure-list-get ()
  "Return a random selection from an adventure list."
  (interactive)
  (let ((list-type (completing-read "Which list? " '("npcs" "threads"))))
    (if (mythic-adventure--list list-type)
	(let ((selection (seq-random-elt (mythic-adventure--list list-type))))
	  (message "%s" selection)
	  selection)
      (mythic-log "%s list is empty" list-type))))

(defun mythic-adventure-list-open ()
  "Opens one of the corresponding adventure lists depending on user selection."
  (interactive)
  (let ((list-type (completing-read "Which list? " '("npcs" "threads"))))
    (find-file (format "%s/%s/lists-%s.org"
		       mythic-adventure-directory
		       mythic-adventure-current
		       list-type))))

;; Util functions for messages and logging:

(defun mythic-log (format-string &rest args)
  "Log a FORMAT-STRING using ARGS to *Mythic GME Log* and the adventure's log file."
  (let ((result (apply #'format format-string args)))
    (unless (not mythic-adventure-current)
      (with-temp-file (format "%s/%s/adventure.log"
			      mythic-adventure-directory
			      mythic-adventure-current)
	(log-view-mode)
	(let ((buffer-read-only nil))
	  (goto-char (point-min))
	  (insert-file-contents (format "%s/%s/adventure.log"
					mythic-adventure-directory
					mythic-adventure-current))
	  (goto-char (point-max))
	  (insert (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
	  (insert ": ")
	  (insert result)
	  (insert "\n"))))
    (with-current-buffer (get-buffer-create "*Mythic GME Log*")
      (log-view-mode)
      (let ((buffer-read-only nil))
	(goto-char (point-max))
	(insert (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
	(insert ": ")
	(insert result)
	(insert "\n")))))

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
    (mythic-log "[%s] + [%s] + {%s} = [%s] @ {%d} => %s"
		die-one die-two roll-modifier (+ die-one die-two roll-modifier) chaos-rank response)
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
    (mythic-log "----------START FATE QUESTION")
    (setq likelihood-modifier (cdr (assoc likelihood
					  mythic-fate-check-modifiers)))
    (setq result
	  (format "QUESTION: %s (%s) => %s"
		  question
		  likelihood
		  (chaos-roll
		   mythic-chaos-factor
		   likelihood-modifier)))
    (if (equal current-prefix-arg nil)
	(message result)
      (insert result))
    (mythic-log result)
    (mythic-log "----------END FATE QUESTION")))


;; Extra meaning results

(defun mythic-description ()
  "Return a random selection from the description part of `mythic-meaning-tables'."
  (interactive)
  (let ((result))
    (setq result
	  (format "%s %s"
		  (seq-random-elt (car (car (cdr mythic-meaning-tables))))
		  (seq-random-elt (car (cdr (car (cdr mythic-meaning-tables)))))))
    (if (equal current-prefix-arg nil)
	(message result)
      (insert result))
    (mythic-log (format "TABLE [DESCRIPTION]: %s" result))))

(defun mythic-action ()
  "Return a random selection from the action part of `mythic-meaning-tables'."
  (interactive)
  (let ((result))
    (setq result
	  (format "%s %s"
		  (seq-random-elt (car (car mythic-meaning-tables)))
		  (seq-random-elt (car (cdr (car mythic-meaning-tables))))))
    (if (equal current-prefix-arg nil)
	(message result)
      (insert result))
    (mythic-log (format "TABLE [ACTION]: %s" result))))

(defun mythic--elements (element)
  "Find 2 ELEMENT's from `mythic-meaning-tables-elements'."
  (let ((result))
    (setq result
	  (format "%s %s"
		  (seq-random-elt
		   (cdr (car (seq-filter #'(lambda (item) (equal (car item) element)) mythic-meaning-tables-elements))))
		  (seq-random-elt
		   (cdr (car (seq-filter #'(lambda (item) (equal (car item) element)) mythic-meaning-tables-elements))))))))

(defun mythic--elements-list ()
  "List the available element tables in `mythic-meaning-tables-elements'."
  (sort (mapcar #'(lambda (item) (car item)) mythic-meaning-tables-elements) #'string<))


(defun mythic-elements ()
  "Return a result from an available element table."
  (interactive)
  (let ((element (completing-read "Which elements table? "
				  (mythic--elements-list))))
    (let ((result (mythic--elements element)))
      (if (equal current-prefix-arg nil)
	  (message result)
	(insert result))
      (mythic-log (format "ELEMENTS [%s]: %s" element result)))))

;; Mythic 2nd Edition chaos factor changes:

(defun chaos-factor-increase ()
  "Increases the chaos factor by one unless it is at the maximum.
It also resets it to the max in case it has somehow gone beyond the max."
  (interactive)
  (if (> mythic-chaos-factor 8)
      (progn
	(mythic-log "Chaos factor is already at max.")
	(setq mythic-chaos-factor 9))
    (progn
      (mythic-log "CHAOS: {%d} -> {%d}" mythic-chaos-factor (+ mythic-chaos-factor 1))
      (setq mythic-chaos-factor (+ mythic-chaos-factor 1)))))

(defun chaos-factor-decrease ()
  "Decreases the chaos factor by one unless it is at the minimum.
It also resets to the minimum in case it has somehow gone beyond the min."
  (interactive)
  (if (< mythic-chaos-factor 2)
      (progn
	(mythic-log "Chaos factor is already at min.")
	(setq mythic-chaos-factor 1))
    (progn
      (mythic-log "CHAOS: {%d} -> {%d}" mythic-chaos-factor (- mythic-chaos-factor 1))
      (setq mythic-chaos-factor (- mythic-chaos-factor 1)))))


;; Event Checks:

(defun mythic-scene--test ()
  "Test the expected scene.  See page 67 in Mythic 2nd Edition."
  (let ((die-roll (roll-die 10))
	(result "Expected Scene"))
    (if (< die-roll mythic-chaos-factor)
	(if (equal (mod die-roll 2) 0)
	    (setq result "Interrupt Scene")
	  (setq result "Altered Scene")))
    (mythic-log "SCENE TEST: [%d] @ {%d} => %s" die-roll mythic-chaos-factor result)
    result))

(defun mythic-scene-test ()
  "Interactive function for testing the next scene.
Run with a prefix argument to insert at point instead of echoing to the
mini-buffer."
  (interactive)
  (let ((result (mythic-scene--test)))
    (if (equal current-prefix-arg nil)
	(message result)
      (insert result))))

(defun mythic-scene-end ()
  "Interactive function for ending a scene."
  (interactive)
  (let ((chaos-decrease (y-or-n-p "Were you in control in this scene?")))
    (if chaos-decrease
	(chaos-factor-decrease)
      (chaos-factor-increase)))
  )


;; Statistics checks:

(defun mythic-statistic--check (expected)
  "Roll a fate question and modify EXPECTED stats accordingly.
See p.127 in Mythic 2nd Edition"
  (let ((answer (chaos-roll mythic-chaos-factor 0))
	(result))
    (cond
     ((and (string-match-p "Yes" answer)
	   (not (string-match-p "Exceptional" answer)))
      (setq result expected))
     ((and (string-match-p "No" answer)
	   (not (string-match-p "Exceptional" answer)))
      (setq expected (* expected 0.75)))
     ((string-match-p "\\(Yes\\)\\(.*Exceptional\\)\\(.*Random Event\\)?" answer) (setq expected (* expected 1.25)))
     ((string-match-p "\\(No\\)\\(.*Exceptional\\)\\(.*Random Event\\)?" answer) (setq expected (* expected 0.5))))
    (if (string-match-p "Random Event" answer)
	(setq result (format "%s (Special Condition)" expected))
      (setq result (format "%s" expected)))
    result))

(defun mythic-statistic-check (actor attribute expected)
  "Interactive function to make a statistics check for ACTOR's ATTRIBUTE.
Depending on results it will be what is EXPECTED, stronger or weaker.
Run with a prefix argument to insert at point instead of echoing to the
mini-buffer."
  (interactive "sActor: \nsAttribute: \nnExpected: ")
  (mythic-log "----------START STATISTICS CHECK")
  (let ((result))
    (setq result
	  (format "%s's %s is %s"
		  actor attribute (mythic-statistic--check expected)))
    (if (equal current-prefix-arg nil)
	(message result)
      (insert result))
    (mythic-log "STATISTICS: %s" result))
  (mythic-log "----------END STATISTICS CHECK"))
    
;; Success rolls:

(defun ttrpg-roll-under-p (target n-dice n-sides)
  "Check if a dice roll with N-DICE and N-SIDES was under or equal to a TARGET."
  (<= (roll-dice-total n-dice n-sides)))

(defun ttrpg-roll-over-p (target n-dice n-sides)
  "Check if a dice roll with N-DICE and N-SIDES was over or equal to a TARGET."
  (>= (roll-dice-total n-dice n-sides)))

;; Transient Menus
(require 'transient)

(transient-define-prefix ttrpg-porcelain ()
  "Main menu of `ttrpg.el' functionality."
  [("d" "roll dice" ttrpg-dice)
   ("m" "Mythic GME" ttrpg-mythic)])

(transient-define-prefix ttrpg-dice ()
  "Dice rolling [WIP]."
  [("u" "under target" (lambda (target) (interactive "nTarget: ") (message "%s" (< (roll-dice-total 3 6) target))))
   ("o" "over target" (lambda (target) (interactive "nTarget: ") (message "%s" (> (roll-dice-total 3 6) target))))])

(transient-define-prefix ttrpg-mythic ()
  "Mythic Game Master Emulator 2nd Edition."
  ["Mythic Information"
   ("" (lambda () (format "Chaos Factor: %s" mythic-chaos-factor)) (lambda () (interactive) (mythic-log mythic-chaos-factor)))
   ("l" "View the Mythic GME Log" (lambda () (interactive) (display-buffer "*Mythic GME Log*")))]
  ["Adventure"]
  ["Scenes"
   ("s n" "New Scene" (lambda () (interactive) (message "Create a new scene")))
   ("s t" "Test Scene" mythic-scene-test)
   ("s e" "End Scene" mythic-scene-end)]
  ["NPCs"
   ("n s" "Statistics Check" mythic-statistic-check)]
  ["Tables"
   ("t a" "Action Meaning" mythic-action)
   ("t d" "Description Meaning" mythic-description)
   ("t e" "Element Meaning" mythic-elements)]
  ["Chaos Factor"
  ("c +" "Increase Chaos Factor" chaos-factor-increase :transient t)
  ("c -" "Decrease Chaos Factor" chaos-factor-decrease :transient t)
  ("c s" "Set Chaos Factor" (lambda (user-input) (interactive "nChaos Factor: ") (setq mythic-chaos-factor user-input))
   :transient t)])

;; Keybinds

(global-set-key (kbd "C-c C-t") #'ttrpg-porcelain)

(provide 'ttrpg)
;;; ttrpg.el ends here
