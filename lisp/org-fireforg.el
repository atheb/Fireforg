;;; org-fireforg.el --- provide functionality for interaction of
;;;                     Fireforg, a Firefox extension, with org mode


;; Copyright 2009 Andreas Burtzlaff
;;
;; Author: Andreas Burtzlaff < andreas at burtz[REMOVE]laff dot de >
;; Version: 0.1alpha3
;; Keywords: org-mode filesystem tree
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;  Protocol:
;;
;;    fireforg-show-annotation://<file (encoded)>/<header (encoded)>
;;    ---
;;      Opens the given file in emacs and searches for header
;;

(require 'org)

(require 'org-protocol)

;;(require 'org-registry)

(add-to-list 'org-protocol-protocol-alist
             '("Fireforg show annotation: fireforg-show-annotation://<file (encoded)>/<header (encoded)>"
              :protocol "fireforg-show-annotation"
              :function org-fireforg-show-annotation))

(add-to-list 'org-protocol-protocol-alist
             '("Fireforg get bibtex entry: fireforg-bibtex-entry://<bibtex entry (encoded)>"
              :protocol "fireforg-bibtex-entry"
              :function org-fireforg-receive-bibtex-entry))


;; Searches for header in given file
(defun org-fireforg-show-annotation (data)
  (let* ((arguments (org-protocol-split-data data t))
         (file (nth 0 arguments))
         (heading (nth 1 arguments)))
        (find-file file)
        (goto-char (point-min))
        (re-search-forward (regexp-quote heading))
        (beginning-of-line)
        (org-show-context)))

;; Renamed functions of rewritten org-registry.el
;; Temporarily moved here to avoid confusing.
;; Subject to change.


;; This needs to be customizable
(defun org-fireforg-registry-file-set () (org-agenda-files))

(defcustom org-fireforg-registry-file
  (concat (getenv "HOME") "/.org-fireforg-registry.el")
  "The Org registry file."
  :group 'org-fireforg-registry
  :type 'file)

(defcustom org-fireforg-registry-file-xml
  (concat (getenv "HOME") "/.org-fireforg-registry.xml")
  "The Org registry file in xml format. Used by fireforg."
  :group 'org-fireforg-registry
  :type 'file)

(defcustom org-fireforg-registry-find-file 'find-file-other-window
  "How to find visit files."
  :type 'function
  :group 'org-fireforg-registry)

(defvar org-fireforg-registry-alist nil
  "An alist containing the Org registry.")


;;;###autoload
(defun org-fireforg-registry-initialize (&optional from-scratch)
  "Initialize `org-fireforg-registry-alist'. 
If FROM-SCRATCH is non-nil or the registry does not exist yet,
create a new registry from scratch and eval it. If the registry
exists, eval `org-fireforg-registry-file' and make it the new value for
`org-fireforg-registry-alist'."
  (interactive "P")
  (message (concat "org-fireforg-registry-initialize: org-agenda-files = " (with-output-to-string (prin1 org-agenda-files)))) ;; DEBUG
  (cond ((or from-scratch (not (file-exists-p org-fireforg-registry-file)))
	  ;; create a new registry
	  (setq org-fireforg-registry-alist nil)
	  (mapc 
	   (lambda (file) 
	     (setq org-fireforg-registry-alist (org-fireforg-registry-get-entries (expand-file-name file) org-fireforg-registry-alist))) 
	   (org-fireforg-registry-file-set))
	  
;;	  (when from-scratch
	    (org-fireforg-registry-create org-fireforg-registry-alist)
	    (org-fireforg-registry-create-xml org-fireforg-registry-alist))
	(t 
	 ;; eval the registry file
	 (with-temp-buffer
	   (insert-file-contents org-fireforg-registry-file)
;;         (eval-buffer) ;; reloading the registry is not working yet. Use (org-fireforg-registry-initialize t) for the time being.
           )
         (org-fireforg-registry-create-xml org-fireforg-registry-alist))))

;;;###autoload
(defun org-fireforg-registry-insinuate ()
  "Call `org-fireforg-registry-update' after saving in Org-mode.
Use with caution.  This could slow down things a bit."
  (interactive)
  (add-hook 'org-mode-hook 
	    (lambda() (add-hook 'after-save-hook 
				'org-fireforg-registry-update t t))))

;; Warning: complex data structure ahead.
(defun org-fireforg-registry-get-entries (file &optional registry)
  "Merge all Org links in FILE into the registry."
  (let (bufstr
        (result registry)
        (add-entry-for (function (lambda (link desc)
				   (let* ((point (match-beginning 0))
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

		       ))

    (with-temp-buffer
      (insert-file-contents file)
      ;; Turn on org-mode in order to use org-heading-components and org-get-tags-at
      ;; This can have severe impact on performance for large files, so I want to get rid of this requirement.
      (org-mode)
      (goto-char (point-min))
      (while (re-search-forward org-bracket-link-regexp nil t)
        (funcall add-entry-for (match-string-no-properties 1) (or (match-string-no-properties 3) "No description")))
      (goto-char (point-min))
      (while (re-search-forward org-angle-link-re nil t)
        (funcall add-entry-for (concat (match-string-no-properties 1) ":" (match-string-no-properties 2)) "" ))
      (goto-char (point-min))
      (while (re-search-forward org-plain-link-re nil t)
        (funcall add-entry-for (match-string-no-properties 0) "" ))
      result)))

;;;###autoload
(defun org-fireforg-registry-update ()
  "Update the registry for the current Org file, if it is in org-fireforg-registry-file-set."
  (interactive)
  (unless (org-mode-p) (error "Not in org-mode"))
  (cond ((not (file-exists-p org-fireforg-registry-file))
         ;; registry-file doesn't exist -> create it from scratch
         (org-fireforg-registry-initialize t))
        (t
	 ;; update existing registry-file
	 (let ((from-file (expand-file-name (buffer-file-name))))
	   (cond ((member from-file (mapcar 'expand-file-name (org-fireforg-registry-file-set)))
		  (let ((registryFiltered (org-fireforg-registry-filter-where-filename-not from-file org-fireforg-registry-alist)))
		    (setq org-fireforg-registry-alist (org-fireforg-registry-get-entries from-file registryFiltered))
		  (org-fireforg-registry-create org-fireforg-registry-alist)
                  (org-fireforg-registry-create-xml org-fireforg-registry-alist)
		  ;;(message (format "Org registry updated for %s. Found %i entries. Registry contains %i entries." (file-name-nondirectory from-file) (length new-entries) (length org-fireforg-registry-alist)))
                  ))
		 (t (message (format "Current file %s is not in org-fireforg-registry-file-set." from-file))))))))

;; requires expanded filename as argument
(defun org-fireforg-registry-filter-where-filename-not (filename registry)
  (cond ((not registry) nil)
        ((nlistp registry) (error "org-fireforg-registry-filter-where-filename-not: argument registry is not a list"))
        (t 
          (let* ((head (car registry))
                 (fileEntries (nth 1 head)))
         (cons (list (car head) (org-fireforg-registry-filter (lambda (entry) (not (string= (expand-file-name (car entry)) filename))) fileEntries)) (org-fireforg-registry-filter-where-filename-not filename (cdr registry)))))))


(defun org-fireforg-registry-create (entries)
  "Create `org-fireforg-registry-file' with ENTRIES."
  (let (entry)
    (with-temp-buffer
      (find-file org-fireforg-registry-file)
      (erase-buffer)
      (insert
         (concat ";; -*- emacs-lisp -*-\n"
	         ";; Org registry\n"
	         ";; You shouldn't try to modify this buffer manually\n\n"
	         "(setq org-fireforg-registry-alist\n"
                 (org-fireforg-registry-to-string-rec org-fireforg-registry-alist) ")"))
	 
      (set-buffer-file-coding-system 'utf-8)
      (save-buffer)
      (kill-buffer (current-buffer))))
  (message "Org registry created"))

(defun org-fireforg-registry-to-string-rec (obj)
  "Return elisp code that generates an object with identical content as the argument"
  (cond ((not obj) "nil")
        ((stringp obj) (concat "\"" obj "\"")) 
        ((nlistp obj) (with-output-to-string (prin1 obj)))
        (t ;; obj is a list
          (concat "(list " (mapconcat 'org-fireforg-registry-to-string-rec obj " ") ")")
         )
  )
)

(defun org-fireforg-registry-create-xml (entries)
  "Create org-fireforg-registry-file-xml with ENTRIES in xml format."
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

    (when (file-writable-p org-fireforg-registry-file-xml)
      (set-buffer-file-coding-system 'utf-8)
      (write-region (point-min)
		    (point-max)
		    org-fireforg-registry-file-xml))))

(defun org-fireforg-registry-filter (condp lst)
  (delq nil
	  (mapcar (lambda (x) (and (funcall condp x) x)) lst)))



(defun org-fireforg-receive-bibtex-entry (data)
  (message "Received bibtex string") ;; DEBUG
  (let* ((arguments (org-protocol-split-data data t))
         (bibtex (nth 0 arguments)))
    (condition-case ex (progn
    (kill-new (org-fireforg-generate-heading (org-fireforg-parse-bibtex-entry bibtex)))
    (message "Saved entry header") 
     )
     'error (message (format "Cuaght exception:  [%s] " ex)))
    nil))

(defun org-fireforg-parse-bibtex-entry (bibtexEntryString)
  (let ((org-fireforg-trim-string (function (lambda (string)
          (replace-regexp-in-string "^[ \t]*" "" (replace-regexp-in-string "[ \t]*$" "" string)))))
        (oneLineString (replace-regexp-in-string "\n" "" bibtexEntryString)))
    (string-match "@\\([^ {]*\\) *{" oneLineString)
    (let* ((entryType (match-string 1 oneLineString))
           (tmpEntryContentStart (match-end 0))
           result)
      (string-match "} *$" oneLineString)
      (let* ((tmpEntryContentEnd (match-beginning 0))
             (entryContent (substring oneLineString tmpEntryContentStart tmpEntryContentEnd))
             (entryContentSplit (split-string entryContent ","))
             (entryId (funcall org-fireforg-trim-string (nth 0 entryContentSplit)))
             (tmp ""))
        (setq result (list (cons "entry_type" entryType) (cons "CUSTOM_ID" entryId)))
        (setq entryContentSplit (cdr entryContentSplit))
        (while entryContentSplit
          (setq tmp (concat tmp (if (= (length tmp) 0) "" ",") (car entryContentSplit)))
          (cond ((and (= (length (split-string tmp "{"))  (length (split-string tmp "}"))) (= (1- (mod (length (split-string tmp "\"")) 2)) 0 ))
                 (string-match "\\([^{\"]+\\)=" tmp)
                 (setq result (cons (cons (funcall org-fireforg-trim-string (match-string 1 tmp)) 
                                          (replace-regexp-in-string "[}\"]$" "" (replace-regexp-in-string "^[{\"]" "" (funcall org-fireforg-trim-string (substring tmp (match-end 0)))))) result))
                 (setq tmp ""))
                (t))
          (setq entryContentSplit (cdr entryContentSplit))) 
        result))))

(defun org-fireforg-headings-to-bibtex (&optional match)
  (reduce 'concat (org-map-entries (lambda () (concat (org-fireforg-heading-to-bibtex-entry) ",\n\n")) match )))

(defun org-fireforg-heading-to-bibtex-entry ()
  (let* ((properties (org-entry-properties))
         (type (cdr (assoc "BIB_entry_type" properties)))
         (id (cdr (assoc "CUSTOM_ID" properties)))
         (properties (rassq-delete-all id (rassq-delete-all type properties))))
    (cond ((and type id)
           (concat "@" type "{" id 
                   (reduce 'concat 
                           (mapcar (lambda (prop) (if (and (> (length (car prop)) 4) (string= (substring (car prop) 0 4) "BIB_")) (concat ",\n  " (substring (car prop) 4) " = {" (cdr prop) "}" ))) properties ) :initial-value "") "\n}")))))

(defun org-fireforg-bibtex-entry-to-properties (bibtexEntry)
  (concat ":PROPERTIES:\n"
   (reduce 'concat (mapcar (lambda (entry) (concat (if (string= (car entry) "CUSTOM_ID") "  :CUSTOM_ID: " (concat "  :BIB_" (car entry) ": ")) (cdr entry) "\n")) bibtexEntry) :initial-value "")
   ":END:"))

(defun org-fireforg-generate-heading (bibtexEntry)
  (let ((heading (concat "* [[" (cdr (assoc "url" bibtexEntry)) "][" (cdr (assoc "title" bibtexEntry)) "]]\n" (org-fireforg-bibtex-entry-to-properties bibtexEntry))))
    (with-temp-buffer (insert heading) (goto-char (point-min)) (org-id-get-create) (buffer-substring (point-min) (point-max)))))

(provide 'org-fireforg)

(defmacro safe-wrap (fn &rest clean-up)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))
