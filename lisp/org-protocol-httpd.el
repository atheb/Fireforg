
(defgroup org-protocol-httpd nil
"A server implementation for interaction with org-mode extending
the capabilities of org-protocol by allowing data to be returned
from Emacs to the client."
:group 'org-protocol)

(defcustom org-protocol-httpd-port 15187
"The port at which the server will be accepting connections."
:group 'org-protocol-httpd
:type 'integer)

(defconst org-protocol-httpd-process-name "org-protocol-httpd")

(defvar org-protocol-httpd-mime-types
  '(("txt"  . "text/plain")
    ("html" . "text/html")
    ("xml"  . "text/xml")))

(defvar org-protocol-httpd-status-strings
  '(( 200 . "OK")
    (204 . "No content")
    (400 . "Bad Request")
    (500 . "Internal Server Error"))
  "Mapping of Http 1.0 status codes to strings.")

(defvar org-protocol-httpd-the-protocol "org-protocol-http")

(defconst org-protocol-httpd-protocol-alist-default nil
  "Default protocols to use with org-protocol-http requests.
See `org-protocol-httpd-protocol-alist' for a description of this variable.")


(defcustom org-protocol-httpd-protocol-alist nil
  "* Register custom handlers for org-protocol-http.

Each element of this list must be of the form:

  (module-name :protocol protocol :function func :kill-client nil)

protocol - protocol to detect in a filename without trailing colon and slashes.
           See rfc1738 section 2.1 for more on this.
           If you define a protocol \"my-protocol\", `org-protocol-check-filename-for-protocol'
           will search filenames for \"org-protocol:/my-protocol:/\"
           and trigger your action for every match. `org-protocol' is defined in
           `org-protocol-the-protocol'. Double and tripple slashes are compressed
           to one by emacsclient.

function - function that handles requests with protocol and takes exactly one
           argument: the filename with all protocols stripped. If the function
           returns nil, emacsclient and -server do nothing. Any non-nil return
           value is considered a valid filename and thus passed to the server.

           `org-protocol.el provides some support for handling those filenames,
           if you stay with the conventions used for the standard handlers in
           `org-protocol-protocol-alist-default'. See `org-protocol-split-data'.

kill-client - If t, kill the client immediately, once the sub-protocol is
           detected. This is neccessary for actions that can be interupted by
           `C-g' to avoid dangeling emacsclients. Note, that all other command
           line arguments but the this one will be discarded, greedy handlers
           still receive the whole list of arguments though.

Here is an example:

  (setq org-protocol-protocol-alist
      '((\"my-protocol\"
         :protocol \"my-protocol\"
         :function my-protocol-handler-fuction)
        (\"your-protocol\"
         :protocol \"your-protocol\"
         :function your-protocol-handler-fuction)))"
  :group 'org-protocol-httpd
  :type '(alist))


(defun org-protocol-httpd-start-server ()
  "Start org-protocols server."
  (interactive)
  (org-protocol-httpd-stop)
  (make-network-process
   :name org-protocol-httpd-process-name
   :server t
   :service org-protocol-httpd-port
   :filter 'org-protocol-httpd-filter
   :family 'ipv4))

(defun org-protocol-httpd-stop ( &optional process-to-stop)
  "Stop org-protocols server."
  (interactive)
  (let ((process (or process-to-stop
                     org-protocol-httpd-process-name)))
    (when (process-status process)
      (process-send-eof process)
      ;; DEBUG
      (message "Stopping process %s" process)
      (delete-process process))))

(defun org-protocol-httpd-filter (process header)
  "Respond to query if the path `string' has valid org-protocol
format and the subprotocol is registered in
`org-protocol-protocol-alist-default'."
  ;;(org-protocol-httpd-send-response process 200 "txt" ""))
  (condition-case err
      (progn
        ;; DEBUG
;;        (message "filter with process: %s " process)
;;        (message "org-protocol-httpd-filter NEW: received: %s" header)
        (let* ((header-alist (org-protocol-httpd-parse-header header))
               (path (car (split-string (replace-regexp-in-string "^/" "" (cdr (assoc "GET" header-alist))) "?")))
               responseString 
               (responseStatus 500))
          ;; DEBUG
;;          (message "org-protocol-httpd-filter NEW: PATH: %s" path)
          (cond ((string-match (concat "^" org-protocol-the-protocol ":") path)
                 ;; let org-protocol handle the path
                 ;; TODO: Find a way to check whether path has been digested without errors
                 ;;       Use an error handler for this?
                 (org-protocol-check-filename-for-protocol path nil nil (lambda () (org-protocol-httpd-stop process)))
                 (org-protocol-httpd-send-response process 200 "txt" ""))
                ((string-match (concat "^" org-protocol-httpd-the-protocol "://\\([^:/]+\\)://\\(.*\\)") path)
;;                 (message "httpd-protocol encountered")
                 (let ((sub-protocol-string (match-string 1 path))
                       sub-protocol-entry responseString)  
                   (when (null sub-protocol-string)
                     (error "Error parsing sub protocol!"))                
                   (setq sub-protocol-entry (assoc sub-protocol-string org-protocol-httpd-protocol-alist))
                   (when (null sub-protocol-entry)
                     (error "No sub protocol." ))
                   (setq sub-protocol-entry (cdr sub-protocol-entry))
;;                   (message "Found sub protocol")
                   (setq responseString 
                         (funcall 
                          (plist-get sub-protocol-entry :function)
                          (match-string 2 path)
                          ))
 ;;                  (message "Function executed")
                   (org-protocol-httpd-send-response process 200 
                                                     (plist-get sub-protocol-entry :mime)
                                                     responseString)))
                (t 
                 (org-protocol-httpd-send-response process 500 "txt" "")))))
    ;; error handler
    (quit 
     (message "Quit in org-protocol-httpd-filter")
     (org-protocol-httpd-send-response process 500 "txt" ""))
    (error
     (org-protocol-httpd-send-response process 500 "txt" "")
     (message "Caught error in org-protocol-httpd-filter: %s" err))))

(defun org-protocol-httpd-parse-header (header)
  "Parse a http header into an alist by splitting each line at the
first space or \": \"."
  (let* ((lines (split-string header "\r?\n"))
         (firstLine (split-string (car lines) " ")))
    (cons
     (cons (car firstLine) (nth 1 firstLine))
     (mapcar 
      (lambda (line) 
        (let ((line-components (split-string line ": " line)))
          (cons 
           (car line-components) 
           (mapconcat 'identity (cdr line-components) ": "))))
      (cdr lines)))))

(defun org-protocol-httpd-send-response (process status mime string)
  "Send a string as response to the currently processed request.
See also `org-protocol-httpd-send-eof'."
  ;; DEBUG
;;  (message "Sending response string.") 
  (process-send-string process 
                       (concat 
                        (org-protocol-httpd-generate-header mime status (length string))
                        string))
  (process-send-eof process)
;;  (message "Deleting process: %s" process)
  (delete-process process))

(defun org-protocol-httpd-send-eof (process)
  (process-send-eof process))

(defun org-protocol-httpd-generate-header (mime status content-length)
  (let ((mime-string (cdr (assoc mime org-protocol-httpd-mime-types)))
        (status-string (cdr (assoc status org-protocol-httpd-status-strings))))
    (when (null mime-string)
      (error "Unknown mime type"))
    (when (null status-string)
      (error "Unknown status type: %d" status))
    (concat "HTTP/1.1 " 
            (number-to-string status)
            " "
            status-string 
            "\nContent-Type: "
            mime-string "\n"
            "Content-length: " (number-to-string content-length) "\n"
            "Connection: close\n"
           "\n")))
