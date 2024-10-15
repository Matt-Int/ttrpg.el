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

(defun gurps-bestiary-entry (name)
  (interactive "sName: ")
  (mapc #'(lambda (text) (insert (format "%s\n" text)))
	`(,(format "#+title: %s" name)
	  "#+filetags: beast:gurps:"
	  "#+OPTIONS: toc:nil num:nil tags:nil title:nil"
	  "#+latex_header: \\usepackage[table]{xcolor}"
	  "#+latex_header: \\usepackage{mdframed, multicol}"
	  "#+latex_header: \\usepackage[symbol, para, multiple]{footmisc}"
	  "#+latex_class_options: [a4paper, twocolumn]"
	  "#+latex_header_extra: \\AtBeginEnvironment{tabular}{\\small}"
	  "#+MACRO: todo @@latex:\\huge@@ @@html:<h1>@@ DONT FORGET TO FILL THIS IN AT SOME POINT @@html:</h1>@@"
	  "#+MACRO: item @@latex:\\item@@ $1 @@html:<br>@@"
	  ,(format "* %s" name)
	  "#+begin_mdframed"
	  "*Overview:* {{{todo}}}"
	  "#+end_mdframed"
	  ""
	  "| *ST:*    | 10 | *HP:*    | 10 | *Speed:*  |   --- |"
	  "| *DX:*    | 10 | *Will:*  | 10 | *Move:*   |    -- |"
	  "| *IQ:*    | 10 | *Per:*   | 10 | *Weigth:* | ----- |"
	  "| *HT:*    | 10 | *FP:*    | 10 | *SM:*     |    +0 |"
	  "| *Dodge:* | -- | *Parry:* | -- | *DR:*     |    -- |"
	  ""
	  "#+begin_description"
	  "{{{item(*Attack (Skill):* damage-dice damage-type. Reach C,1.)}}}"
	  "{{{item(*Traits:* 360° Vision (Easy to hit -20%) =[20]=; ...)}}}"
	  "{{{item(*Skills:* Survival-12)}}}"
	  "{{{item(*Notes:* =[A]= Remove if not needed)}}}"
	  "#+end_description"
	  ""
	  "#+ATTR_LATEX: :height 5.75cm"
	  "#+ATTR_HTML: :width 30%"
	  "/Add image here if there is space/"
	  "@@comment: CREDIT FOR IMAGE HERE@@"
	  "#+latex: \\newpage"
	  "** TYPE Body Type"
	  "#+LATEX: \\rowcolors{2}{}{black!10}"
	  "| Roll (3d) | Use B552-553   | DR | Notes |"
	  "|-----------+----------------+----+-------|"
	  "|        -- | Eyes (-6)      |    |       |"
	  "|       3-4 | Skull (-7)     |    |       |"
	  "|         5 | Face (-5)      |    |       |"
	  "|       6-7 | Right Leg (-2) |    |       |"
	  "|         8 | Right Arn (-2) |    |       |"
	  "|      9-10 | Torso (0)      |    |       |"
	  "|        11 | Groin (-3)     |    |       |"
	  "|        12 | Left Arm (-2)  |    |       |"
	  "|     13-14 | Left Leg (-2)  |    |       |"
	  "|        15 | Hand (-4)      |    |       |"
	  "|        16 | Foot (-4)      |    |       |"
	  "|     17-18 | Neck (-5)      |    |       |"
	  "|         - | Vitals (-3)    |    |       |"
	  "#+begin_group"
	  "#+latex: \\footnotesize"
	  "{{{item(=[1]= Note something here)}}}"
	  "#+end_group"
	  "#+LATEX: \\rowcolors{1}{}{}"
	  "** Details"
	  "Include some more description of the creature here {{{todo}}}"
	  "* COMMENT Brainstorm")))

(provide 'gurps)
;;; gurps.el ends here
