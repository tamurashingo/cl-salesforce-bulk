(defpackage cl-salesforce-bulk.job
  (:use :cl)
  (:import-from :cl-salesforce-bulk.connection
                :<salesforce-connection>
                :post-data)
  (:export :<salesforce-batch>
           :create-job
           :add-batch
           :close-job))
(in-package :cl-salesforce-bulk.job)

(defparameter +OPEN-JOB-XML/EXTERNAL-ID+
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<jobInfo xmlns=\"http://www.force.com/2009/06/asyncapi/dataload\">
  <operation>~A</operation>
  <object>~A</object>
  <externalIdFieldName>~A</externalIdFieldName>
  <contentType>CSV</contentType>
</jobInfo>")

(defparameter +OPEN-JOB-XML+
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<jobInfo xmlns=\"http://www.force.com/2009/06/asyncapi/dataload\">
  <operation>~A</operation>
  <object>~A</object>
  <contentType>CSV</contentType>
</jobInfo>")

(defparameter +CLOSE-JOB-XML+
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<jobInfo xmlns=\"http://www.force.com/2009/06/asyncapi/dataload\">
  <state>Closed</state>
</jobInfo>")

(defclass <salesforce-job> ()
  ((connection :type '<salesforce-connection>
               :initarg :connection
               :accessor connection
               :initform NIL)
   (job-id :initarg :job-id
           :accessor job-id
           :initform NIL)))

(defmethod create-job ((connection <salesforce-connection>) operation object &key (external-id-field-name nil))
  "create a job

returns: <salesforce-job>

arguments:
connection -- salesforce-connection
operation -- delete, insert, query(Bulk V1 type jobs only), queryall(Bulk V1 type jobs only), upsert, update, hardDelete(Bulk V1 type jobs only)
object -- tartget object (e.g. Account)
external-id-field-name -- The name of the external ID field for an upsert"

  (let* ((data (if external-id-field-name
                   (format NIL +OPEN-JOB-XML/EXTERNAL-ID+ operation object external-id-field-name)
                   (format NIL +OPEN-JOB-XML+ operation object)))
         (result (post-data connection "job" data '(("Content-Type" . "text/xml; charset=UTF-8"))))
         (xml (cxml:parse result (cxml-dom:make-dom-builder)))
         (job-id (xpath:with-namespaces (("" "http://www.force.com/2009/06/asyncapi/dataload"))
                     (xpath:string-value (xpath:evaluate "/jobInfo/id" xml)))))
    (make-instance '<salesforce-job>
                   :connection connection
                   :job-id job-id)))

(defmethod add-batch ((job <salesforce-job>) csv)
  "send csv data

returns: batch-id (string)

arguments:
job -- salesforece-job
csv -- csv data
"
  (let* ((path (format NIL "job/~a/batch" (job-id job)))
         (result (post-data (connection job) path csv '(("Content-Type" . "text/csv; charset=UTF-8"))))
         (xml (cxml:parse result (cxml-dom:make-dom-builder))))
    (xpath:with-namespaces (("" "http://www.force.com/2009/06/asyncapi/dataload"))
      (xpath:string-value (xpath:evaluate "/batchInfo/id" xml)))))

(defmethod close-job ((job <salesforce-job>))
  "close the job"
  (let ((path (format NIL "job/~a" (job-id job))))
    (post-data (connection job) path +CLOSE-JOB-XML+ '(("Content-Type" . "text/xml; charset=UTF-8")))))

