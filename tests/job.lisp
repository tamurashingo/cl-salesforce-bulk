(defpackage cl-salesforce-bulk/tests/job
  (:use :cl
        :cl-salesforce-bulk.job
        :mockingbird
        :rove))
(in-package :cl-salesforce-bulk/tests/job)

;; https://developer.salesforce.com/docs/atlas.en-us.api_asynch.meta/api_asynch/asynch_api_quickstart_create_job.htm
(deftest create-job
  (testing "without external-id-field"
    (with-dynamic-stubs ((dex:post "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<jobInfo
   xmlns=\"http://www.force.com/2009/06/asyncapi/dataload\">
  <id>750x0000000005LAAQ</id>
  <operation>insert</operation>
  <object>Contact</object>
  <createdById>005x0000000wPWdAAM</createdById>
  <createdDate>2009-09-01T16:42:46.000Z</createdDate>
  <systemModstamp>2009-09-01T16:42:46.000Z</systemModstamp>
  <state>Open</state>
  <concurrencyMode>Parallel</concurrencyMode>
  <contentType>CSV</contentType>
  <numberBatchesQueued>0</numberBatchesQueued>
  <numberBatchesInProgress>0</numberBatchesInProgress>
  <numberBatchesCompleted>0</numberBatchesCompleted>
  <numberBatchesFailed>0</numberBatchesFailed>
  <numberBatchesTotal>0</numberBatchesTotal>
  <numberRecordsProcessed>0</numberRecordsProcessed>
  <numberRetries>0</numberRetries>
  <apiVersion>47.0</apiVersion>
  <numberRecordsFailed>0</numberRecordsFailed>
  <totalProcessingTime>0</totalProcessingTime>
  <apiActiveProcessingTime>0</apiActiveProcessingTime>
  <apexProcessingTime>0</apexProcessingTime>
</jobInfo>"))
      (let* ((connection (make-instance 'cl-salesforce-bulk.connection:<salesforce-connection>
                                        :session-id "session-id"
                                        :api-version "39.0"
                                        :instance-host "api.salesforce.com"))
             (job (cl-salesforce-bulk.job:create-job connection "insert" "Account"))
             (params (nth-mock-args-for 1 'dex:post)))
        (ok (string= "750x0000000005LAAQ" (cl-salesforce-bulk.job::job-id job)))
        (ok (string= "https://api.salesforce.com/services/async/39.0/job" (car params)))
        (ok (equal '("X-SFDC-Session" . "session-id")
                   (assoc "X-SFDC-Session" (getf (cdr params) :headers) :test #'string=)))
        (ok (equal '("Content-Type" . "text/xml; charset=UTF-8")
                   (assoc "Content-Type" (getf (cdr params) :headers) :test #'string=)))
        (ok (string= "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<jobInfo xmlns=\"http://www.force.com/2009/06/asyncapi/dataload\">
  <operation>insert</operation>
  <object>Account</object>
  <contentType>CSV</contentType>
</jobInfo>"
                     (getf (cdr params) :content))))))
  (testing "with external-id-field"
    (with-dynamic-stubs ((dex:post "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<jobInfo
   xmlns=\"http://www.force.com/2009/06/asyncapi/dataload\">
  <id>750x0000000005LAAQ</id>
  <operation>insert</operation>
  <object>Contact</object>
  <createdById>005x0000000wPWdAAM</createdById>
  <createdDate>2009-09-01T16:42:46.000Z</createdDate>
  <systemModstamp>2009-09-01T16:42:46.000Z</systemModstamp>
  <state>Open</state>
  <concurrencyMode>Parallel</concurrencyMode>
  <contentType>CSV</contentType>
  <numberBatchesQueued>0</numberBatchesQueued>
  <numberBatchesInProgress>0</numberBatchesInProgress>
  <numberBatchesCompleted>0</numberBatchesCompleted>
  <numberBatchesFailed>0</numberBatchesFailed>
  <numberBatchesTotal>0</numberBatchesTotal>
  <numberRecordsProcessed>0</numberRecordsProcessed>
  <numberRetries>0</numberRetries>
  <apiVersion>47.0</apiVersion>
  <numberRecordsFailed>0</numberRecordsFailed>
  <totalProcessingTime>0</totalProcessingTime>
  <apiActiveProcessingTime>0</apiActiveProcessingTime>
  <apexProcessingTime>0</apexProcessingTime>
</jobInfo>"))
      (let* ((connection (make-instance 'cl-salesforce-bulk.connection:<salesforce-connection>
                                        :session-id "session-id"
                                        :api-version "39.0"
                                        :instance-host "api.salesforce.com"))
             (job (cl-salesforce-bulk.job:create-job connection "upsert" "Account" :external-id-field-name "ExternalId__c"))
             (params (nth-mock-args-for 1 'dex:post)))
        (ok (string= "750x0000000005LAAQ" (cl-salesforce-bulk.job::job-id job)))
        (ok (string= "https://api.salesforce.com/services/async/39.0/job" (car params)))
        (ok (equal '("X-SFDC-Session" . "session-id")
                   (assoc "X-SFDC-Session" (getf (cdr params) :headers) :test #'string=)))
        (ok (equal '("Content-Type" . "text/xml; charset=UTF-8")
                   (assoc "Content-Type" (getf (cdr params) :headers) :test #'string=)))
        (ok (string= "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<jobInfo xmlns=\"http://www.force.com/2009/06/asyncapi/dataload\">
  <operation>upsert</operation>
  <object>Account</object>
  <externalIdFieldName>ExternalId__c</externalIdFieldName>
  <contentType>CSV</contentType>
</jobInfo>"
                     (getf (cdr params) :content)))))))

;; https://developer.salesforce.com/docs/atlas.en-us.api_asynch.meta/api_asynch/asynch_api_quickstart_add_batch.htm
(deftest add-batch
  (testing "add csv"
    (with-dynamic-stubs ((dex:post "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<batchInfo
   xmlns=\"http://www.force.com/2009/06/asyncapi/dataload\">
 <id>751x00000000079AAA</id>
 <jobId>750x0000000005LAAQ</jobId>
 <state>Queued</state>
 <createdDate>2009-09-01T17:44:45.000Z</createdDate>
 <systemModstamp>2009-09-01T17:44:45.000Z</systemModstamp>
 <numberRecordsProcessed>0</numberRecordsProcessed>
 <numberRecordsFailed>0</numberRecordsFailed>
 <totalProcessingTime>0</totalProcessingTime>
 <apiActiveProcessingTime>0</apiActiveProcessingTime>
 <apexProcessingTime>0</apexProcessingTime>
</batchInfo>"))
      (let* ((connection (make-instance 'cl-salesforce-bulk.connection:<salesforce-connection>
                                        :session-id "session-id"
                                        :api-version "39.0"
                                        :instance-host "api.salesforce.com"))
             (job (make-instance 'cl-salesforce-bulk.job:<salesforce-job>
                                 :connection connection
                                 :job-id "750x0000000005LAAQ"))
             (batch (cl-salesforce-bulk.job:add-batch job "header1,header2\ndata1,data2"))
             (params (nth-mock-args-for 1 'dex:post)))
        (ok (string= "751x00000000079AAA" batch))
        (ok (string= "https://api.salesforce.com/services/async/39.0/job/750x0000000005LAAQ/batch" (car params)))
        (ok (equal '("X-SFDC-Session" . "session-id")
                   (assoc "X-SFDC-Session" (getf (cdr params) :headers) :test #'string=)))
        (ok (equal '("Content-Type" . "text/csv; charset=UTF-8")
                   (assoc "Content-Type" (getf (cdr params) :headers) :test #'string=)))
        (ok (string= "header1,header2\ndata1,data2"
                     (getf (cdr params) :content)))))))


;; https://developer.salesforce.com/docs/atlas.en-us.api_asynch.meta/api_asynch/asynch_api_quickstart_close_job.htm
(deftest close-job
  (testing "close the job"
    (with-dynamic-stubs ((dex:post "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<jobInfo xmlns=\"http://www.force.com/2009/06/asyncapi/dataload\">
  <state>Closed</state>
</jobInfo>"))
      (let* ((connection (make-instance 'cl-salesforce-bulk.connection:<salesforce-connection>
                                        :session-id "session-id"
                                        :api-version "39.0"
                                        :instance-host "api.salesforce.com"))
             (job (make-instance 'cl-salesforce-bulk.job:<salesforce-job>
                                 :connection connection
                                 :job-id "750x0000000005LAAQ"))
             (result (cl-salesforce-bulk.job:close-job job))
             (params (nth-mock-args-for 1 'dex:post)))
        (declare (ignore result))
        (ok (string= "https://api.salesforce.com/services/async/39.0/job/750x0000000005LAAQ" (car params)))
        (ok (equal '("X-SFDC-Session" . "session-id")
                   (assoc "X-SFDC-Session" (getf (cdr params) :headers) :test #'string=)))
        (ok (equal '("Content-Type" . "text/xml; charset=UTF-8")
                   (assoc "Content-Type" (getf (cdr params) :headers) :test #'string=)))))))















