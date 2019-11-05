(defpackage cl-salesforce-bulk.api
  (:use :cl)
  (:import-from :cl-salesforce-bulk.connection
                :<salesforce-connection>)
  (:import-from :cl-salesforce-bulk.job
                :<salesforce-job>
                :create-job
                :add-batch
                :close-job
                :job-id)
  (:export :bulk/delete
           :bulk/insert
           :bulk/upsert
           :bulk/update))
(in-package :cl-salesforce-bulk.api)

(defmethod bulk/delete ((connection <salesforce-connection>) object csv)
  "delete records"
  (operation connection "delete" object csv))

(defmethod bulk/insert ((connection <salesforce-connection>) object csv)
  "insert recoreds"
  (operation connection "insert" object csv))

(defmethod bulk/upsert ((connection <salesforce-connection>) object csv external-id-field-name)
  "upsert records"
  (operation connection "upsert" object csv :external-id-field-name external-id-field-name))

(defmethod bulk/update ((connection <salesforce-connection>) object csv)
  "update records"
  (operation connection "update" object csv))

(defmethod bulk/query ((connection <salesforce-connection>) object query)
  "query records"
  (operation connection "query" object query))

(defmethod operation ((connection <salesforce-connection>) operation object csv &key (external-id-field-name nil))
  (let* ((job (create-job connection operation object :external-id-field-name external-id-field-name))
         ((batch-id add-batch job csv)))
    (close-job job)
    (when (string= operation "query")
      (get-batch-status job batch-id
    (job-id job)))

