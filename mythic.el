;; mythic.el --- Use Mythic GM Emulator functionality
;; Author: Mattias <mattias1126@protonmail.com>
;; Maintainer: Mattias <mattias1126@protonmail.com>
;; Keywords: ttrpg games
;; Version 0.1.0
;; URL: <todo: include url>
;; Package-Requires: ((emacs "29.0.50") (ttrpg 0.1.0))

;;; Commentary:

;; Provides a set of helper functions for using Mythic GME 2e.
;; This is based on the Mythic Game Master Emulator Second Edition by Tana Pigeon.
;; It is published by Word Mill Games, and licensed for our use under the Creative
;; Commons Attribution-NonCommercial 4.0 International (CC BY-NC 4.0) license.
;; Find out more at www.wordmillgames.com"

;;; Code:

(require 'ttrpg)
(require 'url)

(defgroup mythic nil
  "This is based on the Mythic Game Master Emulator Second Edition by Tana Pigeon.
It is published by Word Mill Games, and licensed for our use under the Creative
Commons Attribution-NonCommercial 4.0 International (CC BY-NC 4.0) license.
Find out more at www.wordmillgames.com"
  :group 'ttrpg)

(defcustom mythic-chaos-factor nil
  "The chaos factor that influences fate checks.  Should be between 1 and 9.
See p. 19, 22 of Mythic GME 2nd Edition."
  :group 'mythic
  :type 'integer)

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
See p.26 of Mythic GME 2nd Edition for modifiers and p.20 for odds examples."
  :group 'mythic
  :type '(alist :key-type (string :tag "Label")
		:value-type (number :tag "Modifier")))

(defcustom mythic-tables-directory (expand-file-name "var/ttrpg.el/mythic/tables" user-emacs-directory)
  "Directory where ttrpg.el looks for action, description, and elements tables."
  :group 'mythic
  :type 'directory)

(defun mythic-tables-install ()
  "Install or update the mythic tables and put them in `mythic-tables-directory'."
  (interactive)
  (let ((urls '("https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/action1.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/action2.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/description1.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/description2.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/adventure-tone.txt"	"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/alien-species-descriptors.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/animal-actions.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/army-descriptors.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/cavern-descriptors.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/character-actions-combat.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/character-actions-general.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/character-appearance.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/character-background.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/character-conversations.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/character-descriptors.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/character-identity.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/character-motivations.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/character-personality.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/character-skills.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/characters.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/character-traits-and-flaws.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/city-descriptors.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/civilization-descriptors.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/creature-abilities.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/creature-descriptors.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/cryptic-message.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/curses.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/domicile-descriptors.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/dungeon-descriptors.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/dungeon-traps.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/forest-descriptors.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/gods.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/legends.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/locations.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/magic-item-descriptions.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/mutation-descriptors.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/names.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/noble-house.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/objects.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/plot-twists.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/powers.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/scavenging-results.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/smells.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/sounds.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/spell-effects.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/starship-descriptors.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/terrain-descriptors.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/undead-descriptors.txt"
		"https://raw.githubusercontent.com/Matt-Int/ttrpg.el/main/tables/meaning-tables/visions-and-dreams.txt"
		))
	(files '("action1.txt" "action2.txt"
		 "description1.txt" "description2.txt"
		 "meaning-tables/adventure-tone.txt"
		 "meaning-tables/alien-species-descriptors.txt"
		 "meaning-tables/animal-actions.txt"
		 "meaning-tables/army-descriptors.txt"
		 "meaning-tables/cavern-descriptors.txt"
		 "meaning-tables/character-actions-combat.txt"
		 "meaning-tables/character-actions-general.txt"
		 "meaning-tables/character-appearance.txt"
		 "meaning-tables/character-background.txt"
		 "meaning-tables/character-conversations.txt"
		 "meaning-tables/character-descriptors.txt"
		 "meaning-tables/character-identity.txt"
		 "meaning-tables/character-motivations.txt"
		 "meaning-tables/character-personality.txt"
		 "meaning-tables/character-skills.txt"
		 "meaning-tables/characters.txt"
		 "meaning-tables/character-traits-and-flaws.txt"
		 "meaning-tables/city-descriptors.txt"
		 "meaning-tables/civilization-descriptors.txt"
		 "meaning-tables/creature-abilities.txt"
		 "meaning-tables/creature-descriptors.txt"
		 "meaning-tables/cryptic-message.txt"
		 "meaning-tables/curses.txt"
		 "meaning-tables/domicile-descriptors.txt"
		 "meaning-tables/dungeon-descriptors.txt"
		 "meaning-tables/dungeon-traps.txt"
		 "meaning-tables/forest-descriptors.txt"
		 "meaning-tables/gods.txt"
		 "meaning-tables/legends.txt"
		 "meaning-tables/locations.txt"
		 "meaning-tables/magic-item-descriptions.txt"
		 "meaning-tables/mutation-descriptors.txt"
		 "meaning-tables/names.txt"
		 "meaning-tables/noble-house.txt"
		 "meaning-tables/objects.txt"
		 "meaning-tables/plot-twists.txt"
		 "meaning-tables/powers.txt"
		 "meaning-tables/scavenging-results.txt"
		 "meaning-tables/smells.txt"
		 "meaning-tables/sounds.txt"
		 "meaning-tables/spell-effects.txt"
		 "meaning-tables/starship-descriptors.txt"
		 "meaning-tables/terrain-descriptors.txt"
		 "meaning-tables/undead-descriptors.txt"
		 "meaning-tables/visions-and-dreams.txt")))
    (while urls
      (url-copy-file (car urls) (expand-file-name (car files) mythic-tables-directory) t 1)
      (setq urls (cdr urls))
      (setq files (cdr files)))))
    

(defcustom mythic-event-focus-table '(((1  5)  .  "Remote Event")
				      ((6  10)  . "Ambiguous Event")
				      ((11 20)  . "New NPC")
				      ((21 40)  . "NPC Action")
				      ((41 45)  . "NPC Negative")
				      ((46 50)  . "NPC Positive")
				      ((51 55)  . "Move Towards a Thread")
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

(defun mythic-event-focus ()
  "Return a result from the `mythic-event-focus-table'."
  (interactive)
  (let ((dice-roll (roll-die 100)))
    (let ((result (cdr (car (seq-filter #'(lambda (line) (between-p dice-roll (car (car line)) (car (cdr (car line)))))
					mythic-event-focus-table)))))
      (if current-prefix-arg
	  (insert result)
	(message result))
    (mythic-log "EVENT FOCUS: %s" result))))


(defun mythic-log (format-string &rest args)
  "Log a FORMAT-STRING using ARGS to *Mythic GME Log* and the adventure's log file."
  (let ((result (apply #'format format-string args)))
    (unless (if (boundp 'mythic-adventure-current) mythic-adventure-current t)
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
    (if (>= (+ die-one die-two roll-modifier) 11)
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
		   (if (eq mythic-chaos-factor nil)
		       (string-to-number (read-answer
			"What is the chaos factor? "
			'(("1" ?1) ("2" ?2) ("3" ?3)
			  ("4" ?4) ("5" ?5) ("6" ?6)
			  ("7" ?7) ("8" ?8) ("9" ?9))))
		     mythic-chaos-factor)
		   likelihood-modifier)))
    (if (equal current-prefix-arg nil)
	(message result)
      (insert result))
    (mythic-log result)
    (mythic-log "----------END FATE QUESTION")))


;; Extra meaning results

(defun mythic-description ()
  "Return random from \"descriptions1.txt\" and \"descriptions2.txt\".
These files should be located in the directory specified in
`mythic-tables-directory'"
  (interactive)
  (let ((result)
	(file-1 (expand-file-name "description1.txt" mythic-tables-directory))
	(file-2 (expand-file-name "description2.txt" mythic-tables-directory)))
    (setq result
	  (format "%s %s"
		  (read-random-line-file file-1)
		  (read-random-line-file file-2)))
    (if (equal current-prefix-arg nil)
	(message result)
      (insert result))
    (mythic-log (format "TABLE [DESCRIPTION]: %s" result))))

(defun mythic-action ()
  "Return a random selection from the action part of `mythic-meaning-tables'."
  (interactive)
  (let ((result)
	(file-1 (expand-file-name "action1.txt" mythic-tables-directory))
	(file-2 (expand-file-name "action2.txt" mythic-tables-directory)))
    (setq result
	  (format "%s %s"
		  (read-random-line-file file-1)
		  (read-random-line-file file-2)))
    (if (equal current-prefix-arg nil)
	(message result)
      (insert result))
    (mythic-log (format "TABLE [ACTION]: %s" result))))

(defun mythic--elements (element)
  "Find 2 ELEMENT's from the \"meaning-tables\" directory.
Located in `mythic-tables-directory'."
  (let ((result))
    (let ((result)
	  (file (expand-file-name element (expand-file-name "meaning-tables/" mythic-tables-directory))))
      (setq result
	    (format "%s %s"
		    (read-random-line-file file)
		    (read-random-line-file file))))))

(defun mythic--elements-list ()
  "List the available element tables in `mythic-meaning-tables-elements'."
  (directory-files (expand-file-name "meaning-tables/" mythic-tables-directory) nil "txt" t))

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

(defun mythic-character-roller ()
  "Interactive function to roll on a number of mythic elements tables.
Insert each roll on a new line, use `yes-or-no-p' to confirm next roll.
Use a prefix argument to skip the confirmation."
  (interactive)
  (let ((continue t)
	(results)
	(tables '("character-descriptors.txt"
		  "character-appearance.txt"
		  "character-identity.txt"
		  "character-skills.txt"
		  "character-background.txt"
		  "character-motivations.txt"
		  "character-personality.txt"
		  "character-traits-and-flaws.txt")))
    (setq results (mapcar #'mythic--elements tables))
    (if current-prefix-arg
	(mapc #'insert (cl-mapcar #'(lambda (table result)
				      (format "%s: %s\n"
					      (car (split-string table "[\.]"))
					      result))
				  tables results))
      (while continue
	(progn (insert (car (split-string (car tables) "[\.]")))
	       (insert ": ")
	       (insert (car results))
	       (setq results (cdr results))
	       (setq tables (cdr tables))
	       (if (eq results nil) (setq continue nil)
		 (setq continue (y-or-n-p "Roll next? ")))
	       (insert "\n"))))))
  ;;(mythic--elements "character-descriptors.txt")))

(provide 'mythic)
;;; mythic.el ends here

