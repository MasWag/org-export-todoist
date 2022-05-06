;;; todoist-org-export.el --- Push org-agenda as tasks of todoist -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Masaki Waga

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Version: HEAD
;; Author: Masaki Waga
;; Keywords: todoist org-mode
;; URL: https://github.com/MasWag/todoist-org-export
;; License: GNU General Public License >= 3

;;; Commentary:

;; This library implements a function to export tasks of org-agenda as
;; tasks of todoist.
;;
;; This file is based on ox-icalendar.el (bundled with org).
;; Most of the functions in this file depend on todoist.el
;; <https://github.com/abrochard/emacs-todoist>.

;;; Setup:

;; You need to set up todoist.el

;;; Usage:

;; 1. =M-x org-agenda= to open a list of agenda
;; 2. =M-x org-current-agenda-export-todoist= to export the current tasks.

;;; Code:

(require 'ox-icalendar)
(require 'todoist)

(defun todoist-org-export--find-todoist-project-id (projects project_name)
  "Find the project id of the project.
This function does not require the set up of todoist's API key.

PROJECTS the list of all projects.
PROJECT_NAME the name of the project."
  (todoist--project-id
   (-first (lambda (project) (equal (todoist--project-name project) project_name))
           projects)))

(defun todoist-org-export--add-task
    (entry summary location description categories timezone class)
  "Add a task in todoist.
This function REQUIRES the set up of todoist's API key.
"

  (let ((start (or (and (memq 'todo-start org-icalendar-use-scheduled)
			(org-element-property :scheduled entry))
		   ;; If we can't use a scheduled time for some
		   ;; reason, start task now.
		   (let ((now (decode-time)))
		     (list 'timestamp
			   (list :type 'active
				 :minute-start (nth 1 now)
				 :hour-start (nth 2 now)
				 :day-start (nth 3 now)
				 :month-start (nth 4 now)
				 :year-start (nth 5 now))))))
        ;; priority: 4 is the highest priority, 1 is the lowest priority
        (priority (+ (- 3 (- org-priority-lowest org-priority-highest))
                     (- org-priority-lowest
                        (or (org-element-property :priority entry)
		            org-priority-default))))
        ;; ID of the proejct. We use Inbox by default.
        (project-id (todoist-org-export--find-todoist-project-id
                     (todoist--get-projects)
                     (or categories "Inbox"))))

    ;; We need to extract a string from a text-property
    (set-text-properties 0 (length summary) nil summary)
    ;; Make the due string
    (setq due (replace-regexp-in-string
               "\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)"
               "\\1-\\2-"
               (replace-regexp-in-string
                ".*:" ""
                (org-icalendar-convert-timestamp start "" nil timezone))))
    ;; We add task only if it is TODO
    (if (eq (org-element-property :todo-type entry) 'todo)
        (todoist--query
         "POST" "/tasks"
         (append `(("content" . ,summary)
                   ("due_string" . ,due)
                   ("project_id" . ,project-id)
                   ("priority" . ,priority)))))
    "DUMMY"))

(defun org-todoist-entry (entry contents info)
  "This function REQUIRES the set up of todoist's API key."
  (unless (org-element-property :footnote-section-p entry)
    (let* ((type (org-element-type entry))
	   ;; Determine contents really associated to the entry.  For
	   ;; a headline, limit them to section, if any.  For an
	   ;; inlinetask, this is every element within the task.
	   (inside
	    (if (eq type 'inlinetask)
		(cons 'org-data (cons nil (org-element-contents entry)))
	      (let ((first (car (org-element-contents entry))))
		(and (eq (org-element-type first) 'section)
		     (cons 'org-data
			   (cons nil (org-element-contents first))))))))
       (let ((todo-type (org-element-property :todo-type entry))
             (summary (org-icalendar-cleanup-string
		       (or (org-element-property :SUMMARY entry)
			   (org-export-data
			    (org-element-property :title entry) info))))
             (priority (org-element-property :priority entry))
	     (loc (org-icalendar-cleanup-string
		   (org-export-get-node-property
		    :LOCATION entry
		    (org-property-inherit-p "LOCATION"))))
	     (class (org-icalendar-cleanup-string
		     (org-export-get-node-property
		      :CLASS entry
		      (org-property-inherit-p "CLASS"))))
	     ;; Build description of the entry from associated section
	     ;; (headline) or contents (inlinetask).
	     (desc
	      (org-icalendar-cleanup-string
	       (or (org-element-property :DESCRIPTION entry)
		   (let ((contents (org-export-data inside info)))
		     (cond
		      ((not (org-string-nw-p contents)) nil)
		      ((wholenump org-icalendar-include-body)
		       (let ((contents (org-trim contents)))
			 (substring
			  contents 0 (min (length contents)
					  org-icalendar-include-body))))
		      (org-icalendar-include-body (org-trim contents)))))))
	     (cat (org-icalendar-get-categories entry info))
	     (tz (org-export-get-node-property
		  :TIMEZONE entry
		  (org-property-inherit-p "TIMEZONE"))))
	  ;; Task: First check if it is appropriate to export it.  If
	  ;; so, call `todoist-org-export--add-task' to transcode it into
	  ;; a "VTODO" component.
	  (when (and todo-type
		     (cl-case (plist-get info :icalendar-include-todo)
		       (all t)
		       (unblocked
			(and (eq type 'headline)
			     (not (org-icalendar-blocked-headline-p
				   entry info))))
		       ((t) (eq todo-type 'todo))))
	    (todoist-org-export--add-task entry summary loc desc cat tz class))))))

(defun org-current-agenda-export-todoist ()
  "Export the current agenda as tasks of todoist.
This function REQUIRES the set up of todoist's API key."
  (interactive)
  (org-export-string-as
   (with-output-to-string
     (save-excursion
       (let ((p (point-min))
	     (seen nil))	;prevent duplicates
	 (while (setq p (next-single-property-change p 'org-hd-marker))
	   (let ((m (get-text-property p 'org-hd-marker)))
	     (when (and m (not (member m seen)))
	       (push m seen)
	       (with-current-buffer (marker-buffer m)
		 (org-with-wide-buffer
		  (goto-char (marker-position m))
		  (princ
		   (org-element-normalize-string
		    (buffer-substring (point)
				      (org-entry-end-position))))))))
	   (forward-line)))))
   'todoist t
   '(:ascii-charset utf-8 :ascii-links-to-notes nil
                    :icalendar-include-todo all)))

;;; Define an org exporter for todoist.
(org-export-define-derived-backend 'todoist 'ascii
  :translate-alist '((clock . ignore)
		     (footnote-definition . ignore)
		     (footnote-reference . ignore)
		     (headline . org-todoist-entry)
                     (inner-template . ignore)
		     (inlinetask . ignore)
		     (planning . ignore)
		     (section . ignore)
		     (template . ignore)))

(provide 'todoist-org-export)
;;; todoist-org-export.el ends here
