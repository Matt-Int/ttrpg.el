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

(defun gurps-calculate-relative-skill-level (type points)
  "Calculate the relative skill level based on TYPE and POINTS.
See B170 for details."
  (let ((buy-skills-table '(("E" . 0)
			    ("A" . -1)
			    ("H" . -2)
			    ("VH" . -3)))
	(point-rate '((1 . 0)
		      (2 . 1)
		      (3 . 1)
		      (4 . 2)))
	(attribute-difficulty (string-split type "/")))
    (if (eq points 0)
	"*"
      (let ((rsl (if (< points 5)
		     (+ (cdr (assoc points point-rate)) (cdr (assoc (car (cdr attribute-difficulty)) buy-skills-table)))
		   (+ (/ (- points 4) 4) 2 (cdr (assoc (car (cdr attribute-difficulty)) buy-skills-table))))))
	(format "%s%s%d" (car attribute-difficulty) (if (< rsl 0) "" "+") rsl))
      )))


(defun gurps-character-sheet (name)
  "Insert a template character with NAME in an org format."
  (interactive "sName: ")
  (mapc #'(lambda (text) (insert (format "%s\n" text)))
	`(,(format "#+title: %s" name)
	  "#+filetags: gurps-character"
	  "#+begin_quote"
	  "Character description / concept"
	  "#+end_quote"
	  "* Attributes"
	  "#+TBLNAME:attributes"
	  "| Attribute | Level | Pts | Additional Attributes | Value | Pts |"
	  "|-----------+-------+-----+-----------------------+-------+-----|"
	  "| ST        |    10 |   0 | HP                    |    10 |   0 |"
	  "| DX        |    10 |   0 |                       |       |     |"
	  "| IQ        |    10 |   0 | Will                  |    10 |   0 |"
	  "|           |       |     | Per                   |    10 |   0 |"
	  "| HT        |    10 |   0 | FP                    |    10 |   0 |"
	  "|-----------+-------+-----+-----------------------+-------+-----|"
	  "| Total     |       |   0 |                       |       |   0 |"
	  "| Total Pts |     0 |     |                       |       |     |"
	  "#+TBLFM: @>>$3=vsum(@<<..@>>>)::@2$3=($-1-10)*10::@3$3=($-1-10)*20::@4$3=($-1-10)*20::@6$3=($-1-10)*10"
	  "#+TBLFM: @>>$6=vsum(@<<..@>>>)::@2$6=($-1-$-4)*2::@4$6=($-1-$-4)*5::@5$6=($-1-@-1$-4)*5::@6$6=($-1-$-4)*3"
	  "#+TBLFM: @8$2=@-1$+1+@-1$>"
	  "** Damage"
	  "| *thr* | B16 |"
	  "| *sw*  | B16 |"
	  "** Move"
	  "#+tblname: move"
	  "| Attribute   | Value | Pts |"
	  "|-------------+-------+-----|"
	  "| Basic Speed |     5 |   0 |"
	  "| Basic Move  |     5 |   0 |"
	  "| Dodge       |     8 |     |"
	  "|-------------+-------+-----|"
	  "| *Total*     |       |   0 |"
	  "#+TBLFM: @2$3=floor((($-1-((remote(attributes,@3$2)+remote(attributes,@6$2))/4)) / 0.25) * 5)"
	  "#+TBLFM: @3$3=floor((($-1-(floor(@-1$-1))))) * 5"
	  "#+TBLFM: @4$2=floor(@-2+3)::@5$3=vsum(@<<..@>>)"
	  "** Encumbrance"
	  "#+tblname: encumbrance"
	  "| Encumbrance           | Level | Weight | Move | Dodge |"
	  "|-----------------------+-------+--------+------+-------|"
	  "| None (0) /= BL/       |     0 |     16 |    5 |     8 |"
	  "| Light (1) /= BLx2/    |     1 |     32 |    4 |     7 |"
	  "| Medium (2) /= BLx3/   |     2 |     48 |    3 |     6 |"
	  "| Heavy (3) /= BLx6/    |     3 |     96 |    2 |     5 |"
	  "| X-Heavy (4) /= BLx10/ |     4 |    160 |    1 |     4 |"
	  "#+TBLFM: @2$3=floor(remote(attributes,@2$2)*remote(attributes,@2$2) / 5)"
	  "#+TBLFM: @3$3=@2$0*2::@4$3=@2$0*3::@5$3=@2$0*6::@6$3=@2$0*10"
	  "#+TBLFM: $4=floor(remote(move, @2$2) * ((5-$-2)/5))"
	  "#+TBLFM: $5=floor(remote(move, @4$2) - ($2/5)*5)"
	  "* Skills"
	  "#+TBLNAME:skills"
	  "| Skill                               | Type | RSL  | Pts | Page Reference | Notes |"
	  "|-------------------------------------+------+------+-----+----------------+-------|"
	  "| Geography/TL7 (Physical/Earth-like) | IQ/A | IQ-6 |   8 | B198           |       |"
	  "|-------------------------------------+------+------+-----+----------------+-------|"
	  "| Total                               |      |      |   8 |                |       |"
	  "#+TBLFM: @>$4=vsum(@<<..@>>)::@<<$3..@>>$3='(gurps-calculate-relative-skill-level $2 (string-to-number $4))"
	  "* Traits"
	  "| Trait | Level | Pts | Ref | Notes |"
	  "|-------+-------+-----+-----+-------|"
	  "|       |       |     |     |       |"
	  "|-------+-------+-----+-----+-------|"
	  "| Total |       |   0 |     |       |"
	  "#+TBLFM: @>$3=vsum(@<<..@>>)"
	  "* Equipment"
	  "** Carried"
	  "** Carried"
	  "| Category | Total Weight | Total Cost |"
	  "|----------+--------------+------------|"
	  "| Armour   |           24 |        370 |"
	  "| Melee    |            4 |         24 |"
	  "| Ranged   |            4 |        150 |"
	  "|----------+--------------+------------|"
	  "| *Total*  |           32 |        544 |"
	  "#+TBLFM: @2$2=remote(armour,@>$6)::@2$3=remote(armour,@>$5)"
	  "#+TBLFM: @3$2=remote(melee,@>$7)::@3$3=remote(armour,@>$6)"
	  "#+TBLFM: @4$2=remote(ranged,@>$2)::@4$3=remote(ranged,@>$3)"
	  "#+TBLFM: @>$2..$3=vsum(@<<..@>>$0)"
	  "*** Armour"
	  "#+tblname:armour"
	  "| TL    | Armour | Location | DR | Cost | Weight | LC | Notes |"
	  "|-------+--------+----------+----+------+--------+----+-------|"
	  "|       |        |          |    |      |        |    |       |"
	  "|-------+--------+----------+----+------+--------+----+-------|"
	  "| Total |        |          |    |    0 |      0 |    |       |"
	  "#+TBLFM: @>$5=vsum(@<<..@>>)::@>$6=vsum(@<<..@>>)"
	  "*** Melee"
	  "#+tblname:melee"
	  "| TL    | Weapon | Damage   | Reach | Parry | Cost | Weight | ST | Notes      |"
	  "|-------+--------+----------+-------+-------+------+--------+----+------------|"
	  "| -     | Punch  | thr-1 cr | C,1   | 0     |      |        |    | [3] B271   |"
	  "| -     | Kick   | thr cr   | C,1   | No    |      |        |    | [3,4] B271 |"
	  "| -     | Teeth  | thr-1 cr | C,1   | No    |      |        |    | [3] B271   |"
	  "|-------+--------+----------+-------+-------+------+--------+----+------------|"
	  "| Total |        |          |       |       |    3 |      4 |    |            |"
	  "#+TBLFM: @>$6..@>$7=vsum(@<<..@>>)"
	  "*** Ranged"
	  "#+tblname:ranged"
	  "| Category                      | Total Weight | Total Cost |"
	  "|-------------------------------+--------------+------------|"
	  "| [[Muscle-Powered Ranged Weapons][Muscle-Powered Ranged Weapons]] |            4 |        150 |"
	  "| [[Hand Grenade and Incendiary][Hand Grenade and Incendiary]]   |            0 |          0 |"
	  "| [[Firearms][Firearms]]                      |            0 |          0 |"
	  "|-------------------------------+--------------+------------|"
	  "| *Total*                       |            4 |        150 |"
	  "#+TBLFM: @2$2=(remote(range-1,@>$6))::@2$3=(remote(range-1,@>$9)"
	  "#+TBLFM: @3$2=(remote(range-2,@>$6))::@3$3=(remote(range-2,@>$9)"
	  "#+TBLFM: @4$2=(remote(range-3,@>$6))::@4$3=(remote(range-3,@>$9)"
	  "#+TBLFM: @>$2..$3=vsum(@<<..@>>)"
	  "**** Muscle-Powered Ranged Weapons"
	  "#+tblname:range-1"
	  "| TL    | Weapon | Damage | Acc | Range | Weight | RoF | Shots | Cost | ST | Bulk | Notes |"
	  "|-------+--------+--------+-----+-------+--------+-----+-------+------+----+------+-------|"
	  "|       |        |        |     |       |        |     |       |      |    |      |       |"
	  "|-------+--------+--------+-----+-------+--------+-----+-------+------+----+------+-------|"
	  "| Total |        |        |     |       |      0 |     |       |    0 |    |      |       |"
	  "#+TBLFM: @>$6=vsum(@<<..@>>);N::@>$9=vsum(@<<..@>>)"
	  "**** Hand Grenade and Incendiary"
	  "#+tblname:range-2"
	  "| TL    | Weapon | Damage | Weight | Fuse | Cost | LC | Notes |"
	  "|-------+--------+--------+--------+------+------+----+-------|"
	  "|       |        |        |        |      |      |    |       |"
	  "|-------+--------+--------+--------+------+------+----+-------|"
	  "| Total |        |        |      0 |      |    0 |    |       |"
	  "#+TBLFM: @>$4=vsum(@<<..@>>);N::@>$6=vsum(@<<..@>>)"
	  "**** Firearms"
	  "#+tblname:range-3"
	  "| TL    | Weapon | Damage | Acc | Range | Weight | RoF | Shots | Cost | ST | Bulk | Rcl | Notes |"
	  "|-------+--------+--------+-----+-------+--------+-----+-------+------+----+------+-----+-------|"
	  "|       |        |        |     |       |        |     |       |      |    |      |     |       |"
	  "|-------+--------+--------+-----+-------+--------+-----+-------+------+----+------+-----+-------|"
	  "| Total |        |        |     |       |      0 |     |       |    0 |    |      |     |       |"
	  "#+TBLFM: @>$6=vsum(@<<..@>>);N::@>$9=vsum(@<<..@>>)"
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
