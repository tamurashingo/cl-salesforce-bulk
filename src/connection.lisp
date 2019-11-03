(defpackage cl-salesforce-bulk.connection
  (:use :cl)
  (:export :<salesforce-connection>
           :login
           :post-data))
(in-package :cl-salesforce-bulk.connection)

(defparameter +LOGIN-HOST+ "https://login.salesforce.com"
  "login host for production mode" )
(defparameter +LOGIN-HOST-SANDBOX+ "https://test.salesforce.com"
  "login host for sandbox mode")
(defparameter +PATH-PREFIX+ "services/async"
  "common prefix for accessing salesforce async api")

(defclass <salesforce-connection> ()
  ((session-id :initarg :session-id
               :accessor session-id
               :initform NIL)
   (server-url :initarg :server-url
               :accessor server-url
               :initform NIL)
   (api-version :initarg :api-version
                :accessor api-version
                :initform NIL)
   (instance-host :initarg :instance-host
                  :accessor instance-host
                  :initform NIL)))

(defun login (username password api-version &key (sandbox-mode nil))
    (let*  ((login-host (if sandbox-mode +LOGIN-HOST-SANDBOX+ +LOGIN-HOST+))
            (path (format NIL "~A/services/Soap/u/~A" login-host api-version))
            (soap-message (format NIL "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<env:Envelope xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"
              xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
              xmlns:env=\"http://schemas.xmlsoap.org/soap/envelope/\">
  <env:Body>
    <n1:login xmlns:n1=\"urn:partner.soap.sforce.com\">
      <n1:username>~A</n1:username>
      <n1:password>~A</n1:password>
    </n1:login>
  </env:Body>
</env:Envelope>" username password))
            (xml-string (dex:post path
                                  :headers '(("Content-Type" . "text/xml; charset=utf-8")
                                             ("SOAPAction" . "login"))
                                  :content soap-message))
            (xml (cxml:parse xml-string (cxml-dom:make-dom-builder))))
      (xpath:with-namespaces (("" "urn:partner.soap.sforce.com"))
        (let* ((session-id (xpath:string-value (xpath:evaluate "//loginResponse/result/sessionId" xml)))
               (server-url (xpath:string-value (xpath:evaluate "//loginResponse/result/serverUrl" xml)))
               (instance-host (format NIL "~A.salesforce.com" (parse-instance server-url))))
          (make-instance '<salesforce-connection>
                         :session-id session-id
                         :server-url server-url
                         :api-version api-version
                         :instance-host instance-host)))))

(defun parse-instance (server-url)
  (multiple-value-bind (a b)
      (ppcre:scan-to-strings "//([a-zA-Z0-9\-\.]{2,}).salesforce" server-url)
    (declare (ignore a))
    (aref b 0)))


(defmethod post-data ((connection <salesforce-connection>) path data headers)
  "post data to salesforce

connection -- salesforce-connection
path -- api path
data -- data
headers -- additional headers

example: create a job
(post-xml connection \"job\" \"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<jobInfo
   xmlns=\"http://www.force.com/2009/06/asyncapi/dataload\">
 <operation>insert</operation>
 <object>Account</object>
 <contentType>CSV</contentType>
</jobInfo>\" '((\"Content-Type\" . \"text/xml; charset=utf-8\")))
"
  (let ((uri (gen-uri connection path))
        (headers (concatenate 'list headers `(("X-SFDC-Session" . ,(session-id connection))))))
    (dex:post uri
              :headers headers
              :content data)))

(defmethod gen-uri ((connection <salesforce-connection>) path)
  (format NIL "https://~A/~A/~A/~A" (instance-host connection) +PATH-PREFIX+ (api-version connection) path))



