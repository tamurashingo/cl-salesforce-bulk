(defpackage cl-salesforce-bulk.connection
  (:use :cl))
(in-package :cl-salesforce-bulk.connection)

(defparameter *LOGIN-HOST* "https://login.salesforce.com"
  "login host for production mode" )
(defparameter *LOGIN-HOST-SANDBOX* "http://test.salesforce.com"
  "login host for sandbox mode")

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
    (let*  ((login-host (if sandbox-mode *LOGIN-HOST-SANDBOX* *LOGIN-HOST*))
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

