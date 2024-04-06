;; mythic-adventures.el --- Using mythic as a framework for tracking adventures.
;; Author: Mattias <mattias1126@protonmail.com>
;; Maintainer: Mattias <mattias1126@protonmail.com>
;; Keywords: ttrpg games
;; Version 0.1.0
;; URL: <todo: include url>
;; Package-Requires: ((emacs "29.0.50") (mythic "0.1.0"))

;;; Commentary:

;; Provides a set of helper functions for running solo-ttrpg games

;;; Code:

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
      (setq mythic-chaos-factor 5)
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
    (setq mythic-adventure-current adventure)
    (message "Remember to set the Chaos Factor!")))

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

(defun mythic-adventure-list--get (list-type)
  "Return a random selection from the LIST-TYPE adventure list."
  (if (mythic-adventure--list list-type)
      (let ((selection (seq-random-elt (mythic-adventure--list list-type))))
	(message "%s" selection)
	selection)
    (mythic-log "%s list is empty" list-type)))

(defun mythic-adventure-list-open ()
  "Opens one of the corresponding adventure lists depending on user selection."
  (interactive)
  (let ((list-type (completing-read "Which list? " '("npcs" "threads"))))
    (find-file (format "%s/%s/lists-%s.org"
		       mythic-adventure-directory
		       mythic-adventure-current
		       list-type))))


(provide 'mythic-adventures)
;;; mythic-adventures.el ends here
