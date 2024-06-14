;; ttrpg.el --- Roll dice and use Mythic GM Emulator functionality
;; Author: Mattias <mattias1126@protonmail.com>
;; Maintainer: Mattias <mattias1126@protonmail.com>
;; Keywords: ttrpg games
;; Version 0.1.0
;; URL: <todo: include url>
;; Package-Requires: ((emacs "29.0.50") (ttrpg "0.1.0"))

;;; Commentary:

;; Provides a set of helper functions for running solo-ttrpg games

;;; Code:

;; GURPS specific rolls:

(require 'ttrpg)

(defun gurps-skill-roll (skill target modifiers)
  "Roll a 3d against a SKILL TARGET with any MODIFIERS."
  (interactive "sSkill: \nnSL: \nnModifiers: ")
  (let ((dice-roll (roll-dice-total 3 6)))
    (let ((result (format "%s-%d + %d = %d [%d] {%d}"
			  skill target modifiers (+ modifiers target) dice-roll
			  (- (+ target modifiers) dice-roll))))
      (if current-prefix-arg
	  (insert result)
	(message result)))))

(defun gurps-character-sheet (name)
  "Insert a template character with NAME in an org format."
  (interactive "sName: ")
  (mapc #'(lambda (text) (insert (format "%s\n" text)))
	`(,(format "#+title: %s" name)
	  "#+filetags: gurps-character"
	  "#+begin_quote"
	  "Character description"
	  "#+end_quote"
	  "* Attributes"
	  "| Attribute | Level | Pts |"
	  "|-----------+-------+-----|"
	  "| ST        |    10 |   0 |"
	  "| DX        |    10 |   0 |"
	  "| IQ        |    10 |   0 |"
	  "| HT        |    10 |   0 |"
	  "| HP        |    10 |   0 |"
	  "| FP        |    10 |   0 |"
	  "- RT: 4"
	  "  - SL: -7"
	  "- AP: 10"
	  "* Skills"
	  "| Skill                               | RSL  | Pts | Page Reference | Notes |"
	  "|-------------------------------------+------+-----+----------------+-------|"
	  "| Geography/TL7 (Physical/Earth-like) | IQ-6 |   0 | B198           |       |"
	  "* Traits"
	  "| Trait | Level | Pts | Ref | Notes |"
	  "|-------+-------+-----+-----+-------|"
	  "|       |       |     |     |       |"
	  "* Equipment"
	  "** Carried"
	  "*** Armour"
	  "| TL | Armour | Location | DR | Cost | Weight | LC | Notes |"
	  "|----+--------+----------+----+------+--------+----+-------|"
	  "|    |        |          |    |      |        |    |       |"
	  "*** Melee"
	  "| TL | Weapon | Damage   | Reach | Parry | Cost | Weight | ST | Notes    |"
	  "|----+--------+----------+-------+-------+------+--------+----+----------|"
	  "| -  | Punch  | thr-1 cr | C     |     0 | -    | -      | -  | [3] B274 |"
	  "*** Ranged"
	  "**** Muscle-Powered Ranged Weapons"
	  "| TL | Weapon | Damage | Acc | Range | Weight | RoF | Shots | Cost | ST | Bulk | Notes |"
	  "|----+--------+--------+-----+-------+--------+-----+-------+------+----+------+-------|"
	  "|    |        |        |     |       |        |     |       |      |    |      |       |"
	  "**** Hand Grenade and Incendiary"
	  "| TL | Weapon | Damage | Weight | Fuse | Cost | LC | Notes |"
	  "|----+--------+--------+--------+------+------+----+-------|"
	  "|    |        |        |        |      |      |    |       |"
	  "**** Firearms"
	  "| TL | Weapon | Damage | Acc | Range | Weight | RoF | Shots | Cost | ST | Bulk | Rcl | Notes |"
	  "|----+--------+--------+-----+-------+--------+-----+-------+------+----+------+-----+-------|"
	  "|    |        |        |     |       |        |     |       |      |    |      |     |       |"
	  "** Other")))

(provide 'gurps)
;;; gurps.el ends here
