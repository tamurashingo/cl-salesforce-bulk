(defpackage cl-salesforce-bulk/tests/api
  (:use :cl
        :rove))
(in-package :cl-salesforce-bulk/tests/api)

(defparameter *insert-record*
  "Name,Description,ExternalId__c
\"Tamu Systems, Inc.\",\"software company\",C00001
tamura shingo,\"software engineer\",P00001
")

(defparameter *update-record*
  "Description,Id
\"software company\",0016F00002AZv91QAD
")

(defparameter *upsert-record*
  "Name,Description,ExternalId__c
\"Tamu Systems, Inc.\",\"update record\",C00001
\"new company\",\"create record\",C00002
")

(defparameter *delete-record*
  "Id
0016F00003SibyfQAB
")


(defparameter *SALESFORCE-USERNAME* (uiop:getenv "SALESFORCE_USERNAME"))
(defparameter *SALESFORCE-PASSWORD* (uiop:getenv "SALESFORCE_PASSWORD"))

(deftest insert
  (testing "insert 2 records"
    (let ((connection (cl-salesforce-bulk.connection:login *SALESFORCE-USERNAME* *SALESFORCE-PASSWORD* "39.0")))
      (cl-salesforce-bulk.api:bulk/insert connection "Account" *insert-record*))))

(deftest update
  (testing "update 1 record"
    (let ((connection (cl-salesforce-bulk.connection:login *SALESFORCE-USERNAME* *SALESFORCE-PASSWORD* "39.0")))
      (cl-salesforce-bulk.api:bulk/update connection "Account" *update-record*))))

(deftest upsert
  (testing "update 1 record and insert 1 record"
    (let ((connection (cl-salesforce-bulk.connection:login *SALESFORCE-USERNAME* *SALESFORCE-PASSWORD* "39.0")))
      (cl-salesforce-bulk.api:bulk/upsert connection "Account" *upsert-record* "ExternalId__c"))))

(deftest delete
  (testing "delete 1 record"
    (let ((connection (cl-salesforce-bulk.connection:login *SALESFORCE-USERNAME* *SALESFORCE-PASSWORD* "39.0")))
      (cl-salesforce-bulk.api:bulk/delete connection "Account" *delete-record*))))

