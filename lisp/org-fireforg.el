





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



(provide 'org-fireforg)