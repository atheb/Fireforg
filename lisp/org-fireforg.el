





;;  Protocol:
;;
;;    fireforg-show-annotation://<file (encoded)>/<header (encoded)>
;;    ---
;;      Opens the given file in emacs and searches for header
;;

(require 'org)

(require 'org-protocol)

(require 'org-registry)

(add-to-list 'org-protocol-protocol-alist
             '("Fireforg show annotation: fireforg-show-annotation://<file (encoded)>/<header (encoded)>"
              :protocol "fireforg-show-annotation"
              :function org-fireforg-show-annotation))

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
(defun org-fireforg-registry-file-set () org-agenda-files)

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
      (write-region (point-min)
		    (point-max)
		    org-fireforg-registry-file-xml))))

(defun org-fireforg-registry-filter (condp lst)
  (delq nil
	  (mapcar (lambda (x) (and (funcall condp x) x)) lst)))







(provide 'org-fireforg)