;;; org-registry.el --- a registry for Org links
;;
;; Copyright 2007, 2008 Bastien Guerry
;; 
;; Emacs Lisp Archive Entry
;; Filename: org-registry.el
;; Version: 0.1alpha
;; Author: Bastien Guerry <bzg AT altern DOT org>
;; Maintainer: Bastien Guerry <bzg AT altern DOT org>
;; Keywords: org, wp, registry
;; Description: Shows Org files where the current buffer is linked
;; URL: http://www.cognition.ens.fr/~guerry/u/org-registry.el
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; This library add a registry to your Org setup.
;;
;; Org files are full of links inserted with `org-store-link'. This links
;; point to e-mail, webpages, files, dirs, info pages, man pages, etc.
;; Actually, they come from potentially *everywhere* since Org lets you
;; define your own storing/following functions.
;;
;; So, what if you are on a e-mail, webpage or whatever and want to know if
;; this buffer has already been linked to somewhere in your agenda files?
;;
;; This is were org-registry comes in handy.
;;
;;     M-x org-registry-show will tell you the name of the file 
;; C-u M-x org-registry-show will directly jump to the file
;;
;; In case there are several files where the link lives in: 
;;
;;     M-x org-registry-show will display them in a new window
;; C-u M-x org-registry-show will prompt for a file to visit
;;
;; Add this to your Org configuration:
;; 
;; (require 'org-registry)
;; (org-registry-initialize)
;;
;; If you want to update the registry with newly inserted links in the
;; current buffer: M-x org-registry-update
;; 
;; If you want this job to be done each time you save an Org buffer,
;; hook 'org-registry-update to the local 'after-save-hook in org-mode:
;; 
;; (org-registry-insinuate)
;;
;;; Comments on the data structure:
;;  * Heading Text [[link][DescriptionHeading]]
;;
;;    Some Text [[link][Description]]
;;
;;    Some more Text [[link][yetAnotherDescription]]
;;
;; Becomes an item in the registry-alis:
;;    (link (
;;           (<file> (
;;                    ("Heading Text [[link][DescriptionHeading]]" <pointOfHeading> <linkInHeading> (<tag1> <tag2> ...) ( (<point> "Description" <isInHeading>) (<point> "yetAnotherDescription" <isInHeading))) 
;;                    (...)
;;                   )
;;           (<file2> ( (...) ...))
;;          )
;;     ...
;;    )
;;
;;
;;
;;
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup org-registry nil
  "A registry for Org."
  :group 'org)

;; (defcustom org-registry-file-set (list "~/tmp/test_fireforg.org" "~/org/wohnungen.org") ;;org-agenda-files
;;   "The set of files the registry is built from."
;;   :group 'org-registry) 

;; This needs to be customizable
(defun org-registry-file-set () org-agenda-files)

(defcustom org-registry-file
  (concat (getenv "HOME") "/.org-registry.el")
  "The Org registry file."
  :group 'org-registry
  :type 'file)

(defcustom org-registry-file-xml
  (concat (getenv "HOME") "/.org-registry.xml")
  "The Org registry file in xml format. Used by fireforg."
  :group 'org-registry
  :type 'file)

(defcustom org-registry-find-file 'find-file-other-window
  "How to find visit files."
  :type 'function
  :group 'org-registry)

(defvar org-registry-alist nil
  "An alist containing the Org registry.")


;;;###autoload
(defun org-registry-initialize (&optional from-scratch)
  "Initialize `org-registry-alist'. 
If FROM-SCRATCH is non-nil or the registry does not exist yet,
create a new registry from scratch and eval it. If the registry
exists, eval `org-registry-file' and make it the new value for
`org-registry-alist'."
  (interactive "P")
  (message (concat "org-registry-initialize: org-agenda-files = " (with-output-to-string (prin1 org-agenda-files)))) ;; DEBUG
  (cond ((or from-scratch (not (file-exists-p org-registry-file)))
	  ;; create a new registry
	  (setq org-registry-alist nil)
	  (mapc 
	   (lambda (file) 
	     (setq org-registry-alist (org-registry-get-entries (expand-file-name file) org-registry-alist))) 
	   (org-registry-file-set))
	  
;;	  (when from-scratch
	    (org-registry-create org-registry-alist)
	    (org-registry-create-xml org-registry-alist))
	(t 
	 ;; eval the registry file
	 (with-temp-buffer
	   (insert-file-contents org-registry-file)
;;         (eval-buffer) ;; reloading the registry is not working yet. Use (org-registry-initialize t) for the time being.
           )
         (org-registry-create-xml org-registry-alist))))

;;;###autoload
(defun org-registry-insinuate ()
  "Call `org-registry-update' after saving in Org-mode.
Use with caution.  This could slow down things a bit."
  (interactive)
  (add-hook 'org-mode-hook 
	    (lambda() (add-hook 'after-save-hook 
				'org-registry-update t t))))

;; Warning: complex data structure ahead.
(defun org-registry-get-entries (file &optional registry)
  "Merge all Org links in FILE into the registry."
  (let (bufstr
        (result registry))
    (with-temp-buffer
      (insert-file-contents file)
      ;; Turn on org-mode in order to use org-heading-components and org-get-tags-at
      ;; This can have severe impact on performance for large files, so I want to get rid of this requirement.
      (org-mode)
      (goto-char (point-min))
      (while (re-search-forward org-bracket-link-regexp nil t)
	(let* ((point (match-beginning 0))
	       (link (match-string-no-properties 1))
	       (desc (or (match-string-no-properties 3) "No description"))
               (onHeading (org-on-heading-p))
               headingPoint
	       (headingAndTags (save-excursion (if (org-before-first-heading-p) "" (org-back-to-heading t) (setq headingPoint (point)) (let ((ohc (org-heading-components))) (list (nth 4 ohc) (org-get-tags-at))))))
               (heading (car headingAndTags))
               (tags (nth 1 headingAndTags))
               (contentEntry (list point desc onHeading))
               (headingEntry (list heading headingPoint onHeading tags (list contentEntry)))
               (fileEntry (list (expand-file-name file) (list headingEntry)))
               (linkEntry (list link (list fileEntry)))
               (existingLinkEntry (assoc link result))
               (existingFileEntry (assoc (expand-file-name file) (nth 1 existingLinkEntry)))
               (existingHeadingEntry (assoc heading (nth 1 existingFileEntry))))

	  (cond (existingLinkEntry
                 (cond (existingFileEntry
                          (cond (existingHeadingEntry (setf (nth 4 existingHeadingEntry) (cons contentEntry (nth 4 existingHeadingEntry))))
		              (t (setf (nth 1 existingFileEntry) (cons headingEntry (nth 1 existingFileEntry))))))
                       (t (setf (nth 1 existingLinkEntry) (cons fileEntry (nth 1 existingLinkEntry))))))
		 (t (add-to-list 'result linkEntry))))))
    ;; TODO add search for angle links
    ;;      (goto-char (point-min))
    ;;      (while (re-search-forward org-bracket-link-regexp nil t)
    ;;	(let* ((point (match-beginning 0))
    ;;	       (link (match-string-no-properties 1))
    ;;	       (desc (or (match-string-no-properties 3) "No description")))
    result))

;;;###autoload
(defun org-registry-update ()
  "Update the registry for the current Org file, if it is in org-registry-file-set."
  (interactive)
  (unless (org-mode-p) (error "Not in org-mode"))
  (cond ((not (file-exists-p org-registry-file))
         ;; registry-file doesn't exist -> create it from scratch
         (org-registry-initialize t))
        (t
	 ;; update existing registry-file
	 (let ((from-file (expand-file-name (buffer-file-name))))
	   (cond ((member from-file (mapcar 'expand-file-name (org-registry-file-set)))
		  (let ((registryFiltered (org-registry-filter-where-filename-not from-file org-registry-alist)))
		    (setq org-registry-alist (org-registry-get-entries from-file registryFiltered))
		  (org-registry-create org-registry-alist)
                  (org-registry-create-xml org-registry-alist)
		  ;;(message (format "Org registry updated for %s. Found %i entries. Registry contains %i entries." (file-name-nondirectory from-file) (length new-entries) (length org-registry-alist)))
                  ))
		 (t (message (format "Current file %s is not in org-registry-file-set." from-file))))))))

;; requires expanded filename as argument
(defun org-registry-filter-where-filename-not (filename registry)
  (cond ((not registry) nil)
        ((nlistp registry) (error "org-registry-filter-where-filename-not: argument registry is not a list"))
        (t 
          (let* ((head (car registry))
                 (fileEntries (nth 1 head)))
         (cons (list (car head) (org-registry-filter (lambda (entry) (not (string= (expand-file-name (car entry)) filename))) fileEntries)) (org-registry-filter-where-filename-not filename (cdr registry)))))))


(defun org-registry-create (entries)
  "Create `org-registry-file' with ENTRIES."
  (let (entry)
    (with-temp-buffer
      (find-file org-registry-file)
      (erase-buffer)
      (insert
         (concat ";; -*- emacs-lisp -*-\n"
	         ";; Org registry\n"
	         ";; You shouldn't try to modify this buffer manually\n\n"
	         "(setq org-registry-alist\n"
                 (org-registry-to-string-rec org-registry-alist) ")"))
	 
      (save-buffer)
      (kill-buffer (current-buffer))))
  (message "Org registry created"))

(defun org-registry-to-string-rec (obj)
  "Return elisp code that generates an object with identical content as the argument"
  (cond ((not obj) "nil")
        ((stringp obj) (concat "\"" obj "\"")) 
        ((nlistp obj) (with-output-to-string (prin1 obj)))
        (t ;; obj is a list
          (concat "(list " (mapconcat 'org-registry-to-string-rec obj " ") ")")
         )
  )
)

(defun org-registry-create-xml (entries)
  "Create org-registry-file-xml with ENTRIES in xml format."
  (with-temp-buffer
    (insert 
     (concat "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\" ?>\n<orgregistry>\n" 
	     (reduce 'concat (mapcar (lambda (linkEntry)
				       (concat "<link url=\"" (url-insert-entities-in-string (nth 0 linkEntry)) "\">\n"
					       (reduce 'concat (mapcar (lambda (fileEntry) 
									 (let ((file (nth 0 fileEntry)))
									   (reduce 'concat (mapcar (lambda (headingEntry) 
												     (concat "<heading file=\"" (url-insert-entities-in-string file) "\" "
													     "text=\""          (url-insert-entities-in-string (nth 0 headingEntry)) "\" "
													     "linkInHeading=\"" (url-insert-entities-in-string (if (nth 2 headingEntry) "t" "f")) "\" "
													     "tags=\""          (if (nth 3 headingEntry)
                                                                                                                                    (url-insert-entities-in-string (concat ":" (mapconcat 'identity (nth 3 headingEntry) ":") ":"))
                                                                                                                                    "") "\" "
													     ">"
													     (reduce 'concat (mapcar (lambda (contentEntry) 
																       (concat "<contentEntry point=\"" (number-to-string (nth 0 contentEntry)) "\" "
																	       "description=\"" (url-insert-entities-in-string (nth 1 contentEntry)) "\" "
																	       "inHeading=\"" (if (nth 2 contentEntry) "t" "f") "\"/>"))
																     (nth 4 headingEntry)) :initial-value "")
													     "</heading>"))
												   (nth 1 fileEntry)) :initial-value "")
									   )
									 ) (nth 1 linkEntry)) :initial-value "")
					       "</link>")) entries) :initial-value "")
	     "</orgregistry>"))

    (when (file-writable-p org-registry-file-xml)
      (write-region (point-min)
		    (point-max)
		    org-registry-file-xml))))

(defun org-registry-filter (condp lst)
  (delq nil
	  (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(provide 'org-registry)

;;;  User Options, Variables

;;; org-registry.el ends here
