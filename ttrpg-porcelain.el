;; ttrpg-porcelain.el --- Porcelain to handle ttrpg and module activity -*- lexical-binding: t -*-
;; Author: Mattias <mattias1126@protonmail.com>
;; Maintainer: Mattias <mattias1126@protonmail.com>
;; Keywords: ttrpg games
;; Version 0.1.0
;; URL: <todo: include url>
;; Package-Requires: ((emacs "29.0.50") (transient))

;;; Commentary:

;; Provides a set of helper functions for running solo-ttrpg games

;;; Code:

(require 'ttrpg)
(require 'transient)

(transient-define-prefix ttrpg-porcelain ()
  "Main menu of `ttrpg.el' functionality."
  [("d" "roll dice" ttrpg-dice)
   ("m" "Mythic GME" ttrpg-mythic)])

(transient-define-prefix ttrpg-dice ()
  "Dice rolling [WIP]."
  [("g s" "GURPS Skill Roll" gurps-skill-roll)
   ("o" "over target" (lambda (target) (interactive "nTarget: ") (message "%s" (> (roll-dice-total 3 6) target))))
   ("d 4" "D4s" (lambda () (interactive) (message "%s" (roll-dice-total
						   (if current-prefix-arg current-prefix-arg 1)
						   6))))
   ("d 6" "D6s" (lambda () (interactive) (message "%s" (roll-dice-total
						   (if current-prefix-arg current-prefix-arg 1)
						   6))))
   ("d 8" "D8s" (lambda () (interactive) (message "%s" (roll-dice-total
						   (if current-prefix-arg current-prefix-arg 1)
						   6))))
   ("d 10" "D10s" (lambda () (interactive) (message "%s" (roll-dice-total
						   (if current-prefix-arg current-prefix-arg 1)
						   6))))
   ("d 12" "D12s" (lambda () (interactive) (message "%s" (roll-dice-total
						   (if current-prefix-arg current-prefix-arg 1)
						   6))))
   ("d 20" "D20s" (lambda () (interactive) (message "%s" (roll-dice-total
						   (if current-prefix-arg current-prefix-arg 1)
						   6))))
   ])

(transient-define-prefix ttrpg-mythic ()
  "Mythic Game Master Emulator 2nd Edition"
  ["Mythic"
   ["Questions"
    ("o q" "Oracle Question" fate-check)]
   ["Scenes"
    ("s t" "Test Scene" mythic-scene-test)
    ("s f" "Scene Focus" mythic-event-focus)
    ("s e" "End Scene" mythic-scene-end)]
   ["NPCs"
    ("n s" "Statistics Check" mythic-statistic-check)
    ("n c" "Roll on character-* tables" mythic-character-roller)]
   ["Tables"
    ("t a" "Action Meaning" mythic-action)
    ("t d" "Description Meaning" mythic-description)
    ("t e" "Element Meaning" mythic-elements)]
   [:description (lambda () (format "Chaos Factor: %s" mythic-chaos-factor))
    ("c +" "Increase Chaos Factor" chaos-factor-increase :transient t)
    ("c -" "Decrease Chaos Factor" chaos-factor-decrease :transient t)
    ("c s" "Set Chaos Factor" (lambda (user-input) (interactive "nChaos Factor: ") (setq mythic-chaos-factor user-input)))
   ]])

(provide 'ttrpg-porcelain)
;;; ttrpg-porcelain.el ends here
