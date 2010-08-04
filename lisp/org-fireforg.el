;;; org-fireforg.el --- provide functionality for interaction of
;;;                     Fireforg, a Firefox extension, with org mode


;; Copyright 2009 Andreas Burtzlaff
;;
;; Author: Andreas Burtzlaff < andreas at burtzlaff dot de >
;; Version: 0.1alpha13
;; Keywords: org-mode firefox annotations
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
;;    fireforg-bibtex-entry://<BibTeX entry (encoded)>
;;    ---
;;      Sends a BibTeX entry that is formatted according to
;;      `org-fireforg-received-bibtex-format'
;;      and puts into the kill ring


(require 'org)

(require 'org-protocol)

(require 'org-protocol-httpd)

(require 'bibtex)

(add-to-list 'org-protocol-protocol-alist
             '("Fireforg show annotation: fireforg-show-annotation://<file (encoded)>/<header (encoded)>"
               :protocol "fireforg-show-annotation"
               :function org-fireforg-show-annotation))

(add-to-list 'org-protocol-protocol-alist
             '("Fireforg get bibtex entry: fireforg-bibtex-entry://<bibtex entry (encoded)>"
               :protocol "fireforg-bibtex-entry"
               :function org-fireforg-receive-bibtex-entry))

(cond ((boundp 'org-protocol-httpd-protocol-alist)
       (add-to-list 'org-protocol-httpd-protocol-alist
                    '("get-org-subtree" 
                      :protocol "get-org-subtree" 
                      :mime "text/xml"
                      :function org-fireforg-get-org-subtree))
       (add-to-list 'org-protocol-httpd-protocol-alist
                    '("get-annotations-for-url" 
                      :protocol "get-annotations-for-url" 
                      :mime "text/xml"
                      :function org-fireforg-get-annotations-for-url)))
      (t
       (message "org-protocol-httpd no loaded!")))


(defgroup org-fireforg nil
  "Options for the Fireforg extension of Org-mode."
  :group 'org)

(defcustom org-fireforg-received-bibtex-format 'heading
  "Non-nil means, transform bibtex entries.

  If the variable is `headers' the entry is transformed into a
  heading with the bibtex entries as properties prefixed with
  `BIB_'. The CUSTOM_ID is set to the bibtex key."
  :group 'org-fireforg
  :type '(choice
	  (const :tag "Create heading with properties" heading)
	  (const :tag "BibTex" nil)
          (const :tag "Create heading with properties and BibTeX entry as content" headingWithPropsAndBibTeXContent)
          (const :tag "Create heading with BibTeX entry as content" headingWithBibTeXContent)))

(defcustom org-fireforg-locations nil
  "A list of locations for bookmark synchronization.
A location is a heading in an org file, where bookmarks are to be
stored. The elements of this list are 2-element lists. For each
entry the first element is a string containing the name of the
location.  The second entry is a string, that has the same format
as the first part in an org link.

Example: To reference the heading Bookmarks in the file
~/org/bookmarks.org use:
(\"bookmark_location\" \"~/org/bookmarks.org::Bookmarks\") 
";;"
:type '(repeat
        :tag "enabled"
        (list :value ("" "")
              (string :tag "Name")	
              (string :tag "Reference"))))

(defun org-fireforg-initialize ()
  (interactive)
  (add-hook 'org-registry-get-entries-hook 'org-fireforg-get-doi-entries))

;; Searches for header in given file
(defun org-fireforg-show-annotation (data)
  (let* ((arguments (org-protocol-split-data data t))
         (file (nth 0 arguments))
         (heading (nth 1 arguments))
         (heading-point (string-to-number (nth 2 arguments)))
         (frameList (or (visible-frame-list) (frame-list))))
    (find-file file)
    (goto-char (point-min))
    (unless (= 0 heading-point)
      (re-search-forward (regexp-quote heading))
      (beginning-of-line)
      (org-show-context))
    (if frameList (select-frame-set-input-focus (car frameList)))
    ))


(defun org-fireforg-get-annotations-for-url (data)
  (let* ((arguments (org-protocol-split-data data t))
         (url (nth 0 arguments)))
    (message "org-fireforg-get-annotations-for-url: %s" url)
    ;; export registry entries to xml
    (concat
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
     "<org-fireforg-get-annotations-for-url>\n"
     (mapconcat 
      (lambda (entry) 
        (format "<heading file=\"%s\" text=\"%s\" tags=\"%s\" point=\"%d\"/>\n"
                (org-protocol-httpd-escape-xml-attribute-chars (nth 3 entry))
                (org-protocol-httpd-escape-xml-attribute-chars (nth 4 (nth 4 entry)))
                (org-protocol-httpd-escape-xml-attribute-chars (or (nth 5 (nth 4 entry)) ""))
                (nth 5 entry)))
      (org-registry-assoc-all url) "")
     "</org-fireforg-get-annotations-for-url>")))



;; BUG: buffer not reset
(defun org-fireforg-get-org-subtree (data)
  (message "org-fireforg-get-org-subtree with arguments: %s" data)
  (save-current-buffer
    (save-excursion
      (let* ((arguments (org-protocol-split-data data t))
             (location-name (nth 0 arguments)))
        (org-fireforg-do-at-location 
         location-name 
         (lambda ()
           (concat "<location name=\"" location-name "\">\n"
                   (org-fireforg-subtree-to-xml)
                   "</location>")))))))

(defun org-fireforg-subtree-to-xml ()
  (let (result link-position)
    (beginning-of-line 2)
    (narrow-to-region (point) (save-excursion (org-end-of-subtree)))
    (setq result 
          (mapconcat 
           'identity 
           (org-map-entries 
            (lambda ()
              (let ((heading-components (org-heading-components))
                    link)
                (setq 
                 link-position 
                 (re-search-forward org-bracket-link-regexp 
                                    (save-excursion (org-end-of-subtree)) t))
                (cond (link-position
                       (setq link (org-match-string-no-properties 1))
                       (format "<heading title=\"%s\" url=\"%s\"/>\n" 
                               (nth 4 heading-components) 
                               link))
                      (t nil))
                ))) ""))
    (widen)
    result))

(defun org-fireforg-do-at-location (location-name func)
  (let ((location-entry (assoc location-name org-fireforg-locations))
        (currentBuffer (current-buffer))
        return-value)
    (when (null location-entry)
      (error "Cannot find location: %s" location-name))
    (with-temp-buffer 
      ;;      (insert (concat "[[" (nth 1 location-entry) "]]"))
      ;;      (beginning-of-line)
      ;;      (org-open-at-point)
      (org-id-goto (cdr location-entry))
      (setq return-value (funcall func)))
    (set-buffer currentBuffer)
    return-value))
;;    (org-open-link-global (concat "[[" (nth 1 location-entry) "]]"))))

(defun org-fireforg-get-doi-entries (doi-file)
  "Collect all DOI entries from the current buffer."
  ;; returnList is defined in org-registry-get-entries and serves as a
  ;;  return value
  ;; (message "org-fireforg-get-doi-entries called for
  ;;  file: %s" file)
  (goto-char (point-min))
  (while (re-search-forward "BIB_doi" nil t)
    (unless (org-before-first-heading-p)
      (save-excursion
        (org-back-to-heading)
        (let ((doi (org-entry-get (point) "BIB_doi")))
          ;;(message (concat "current file:" currentFile))
          (when doi 
            (setq returnList 
                  (cons 
                   (list 
                    ;; link
                    ;; Due to a bug in Zotero it might happen that
                    ;; the doi identifier is enclosed in two sets
                    ;; of "{}" brackets.1
                    ;; Therefore the function
                    ;; org-fireforg-bibtex-trim-string is apply two
                    ;; times.
                    (org-fireforg-doi-to-url 
                     (org-fireforg-bibtex-trim-string 
                      (org-fireforg-bibtex-trim-string doi)))
                    ;; description
                    "DOI Link"
                    ;; point
                    (point)
                    doi-file
                    (org-heading-components)
                    (point)) returnList)))))))
  returnList)

(defun org-fireforg-receive-bibtex-entry (data)
  ;;(message "Received bibtex string") ;; DEBUG
  (let* ((arguments (org-protocol-split-data data t))
         (bibtex (nth 0 arguments))
         (bibtexParsed (org-fireforg-parse-bibtex-entry-wrapper bibtex)))
    (kill-new 
     (cond ((eq org-fireforg-received-bibtex-format 'heading)
            (concat (org-fireforg-generate-heading bibtexParsed) "\n" 
                    (org-fireforg-bibtex-entry-to-properties bibtexParsed)))
           ((eq org-fireforg-received-bibtex-format 'headingWithPropsAndBibTeXContent)
            (concat (org-fireforg-generate-heading bibtexParsed) "\n" 
                    (org-fireforg-bibtex-entry-to-properties bibtexParsed) bibtex "\n"))
           ((eq org-fireforg-received-bibtex-format 'headingWithBibTeXContent)
            (concat (org-fireforg-generate-heading bibtexParsed) "\n" bibtex "\n"))
           ((not org-fireforg-received-bibtex-format) bibtex)))
    (message "Saved BibTeX entry to kill ring."))
  nil)

(defun org-fireforg-parse-bibtex-entry-wrapper (bibtexEntryString)
  (with-temp-buffer
    (insert bibtexEntryString)
    (goto-char (point-min))
    ;;(bibtex-next-field t)
    (re-search-forward "@")
    (goto-char (match-beginning 0))
    (bibtex-parse-entry)))

(defun org-fireforg-bibtex-trim-string (string)
  (replace-regexp-in-string 
   "[\"}] *$" "" 
   (replace-regexp-in-string 
    "^ *[{\"]" "" 
    string)))

(defun org-fireforg-headings-to-bibtex (&optional match)
  (reduce 'concat 
          (org-map-entries 
           (lambda () 
             (concat (org-fireforg-heading-to-bibtex-entry) "\n\n"))
           match)))

(defun org-fireforg-heading-to-bibtex-entry ()
  (let* ((properties (org-entry-properties))
         (type (cdr (assoc "BIB_entryType" properties)))
         (id (cdr (assoc "CUSTOM_ID" properties)))
         (properties (rassq-delete-all id (rassq-delete-all type properties))))
    (cond ((and type id)
           (concat "@" type "{" id 
                   (reduce 'concat 
                           (mapcar (lambda (prop) (if (and (> (length (car prop)) 4) (string= (substring (car prop) 0 4) "BIB_")) (concat ",\n  " (substring (car prop) 4) " = " (cdr prop) ))) properties ) :initial-value "") "\n}")))))

(defun org-fireforg-bibtex-entry-to-properties (bibtexEntry)
  (concat ":PROPERTIES:\n"
          (reduce 'concat (mapcar (lambda (entry) 
                                    (concat (cond ((string= (car entry) "=key=") "  :CUSTOM_ID")
                                                  ((string= (car entry) "=type=") "  :BIB_entryType")
                                                  (t (concat "  :BIB_" (car entry) ))) ": " (cdr entry) "\n")) (nreverse bibtexEntry)) :initial-value "")
          ":END:\n"))

(defun org-fireforg-generate-heading (bibtexEntry)
  (let* ((url (cdr (assoc "url" bibtexEntry)))
         (heading 
          (concat "* [" 
                  (when url (concat "[" (org-fireforg-bibtex-trim-string url) "]"))
                  "[" (org-fireforg-bibtex-trim-string (cdr (assoc "title" bibtexEntry))) "]]" )))
    ;;(with-temp-buffer (insert heading) (goto-char (point-min)) (org-id-get-create) (buffer-substring (point-min) (point-max)))
    heading
    ))

(defun org-fireforg-import-bibtex-file (file) 
  (interactive "F")
  (save-excursion
    (let ((curBuffer (current-buffer))
          (tmpBuffer (generate-new-buffer "* fireforg tmp *"))
          bibtexEntry entryString)
      (switch-to-buffer tmpBuffer)
      (insert-file-contents file)
      (goto-char (point-min))
      (while (< (point) (point-max))        
        (re-search-forward "@")
        (goto-char (match-beginning 0))
        (cond ((or 
                (looking-at "@string")
                (looking-at "@comment")
                (looking-at "@preamble"))
               (forward-char))
              (t
               (setq bibtexEntry (bibtex-parse-entry))
               (switch-to-buffer curBuffer)
               (insert (org-fireforg-generate-heading bibtexEntry))
               (insert "\n")
               (insert (org-fireforg-bibtex-entry-to-properties bibtexEntry))
               ))
        (switch-to-buffer tmpBuffer)))))

;; exports the bibtex properties of the current buffer to selectable file
(defun org-fireforg-export-bibtex-to-file (file)
  (interactive "F")
  (save-excursion 
    (cond ((not file) (error "No file supplied"))
          ((let ((bibtex (org-fireforg-headings-to-bibtex)))
             (with-temp-buffer 
               (insert bibtex)
               (write-file file t)))))))

(defun org-fireforg-export-bibtex-to-new-buffer ()
  (interactive)
  (let ((bibtex (org-fireforg-headings-to-bibtex))
        ;; find nearest bibliography entry before point
        (prevId (save-excursion 
                  (while (and (not (org-before-first-heading-p))
                              (if (org-at-heading-p) 
                                  (or (null (org-entry-get (point) "CUSTOM_ID"))
                                      (string= (org-entry-get (point) "CUSTOM_ID") ""))
                                1) 
                              (not (bobp)))
                    (progn (backward-char) (org-back-to-heading t)))
                  (if (or (org-before-first-heading-p)
                          (bobp))
                      nil 
                    (org-entry-get (point) "CUSTOM_ID")))))
    (switch-to-buffer (generate-new-buffer "*BibTeX export*"))
    (insert bibtex)
    (goto-char (point-min))
    (when prevId 
      (progn (re-search-forward (regexp-quote prevId))
             (beginning-of-line)))
    (bibtex-mode)))

(defun org-fireforg-doi-to-url (string)
       (concat "http://dx.doi.org/" 
 (replace-regexp-in-string " " "%20"
 (replace-regexp-in-string "#" "%23"
 (replace-regexp-in-string "\"" "%22" 
 (replace-regexp-in-string "%" "%25" string))))))

(provide 'org-fireforg)
