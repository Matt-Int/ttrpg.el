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

(provide 'gurps)
;;; gurps.el ends here
