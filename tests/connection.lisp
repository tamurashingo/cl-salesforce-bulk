(defpackage cl-salesforce-bulk/tests/connection
  (:use :cl
        :cl-salesforce-bulk.connection
        :mockingbird
        :rove))
(in-package :cl-salesforce-bulk/tests/connection)



(deftest login
  (testing "cheking request uri and response"
    (with-dynamic-stubs ((dex:post "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"
                  xmlns=\"urn:partner.soap.sforce.com\"
                  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
  <soapenv:Body>
    <loginResponse>
      <result>
        <metadataServerUrl>https://tamu-dev-ed.my.salesforce.com/services/Soap/m/39.0/xxxxxxxxxxxxxxxx</metadataServerUrl>
        <passwordExpired>false</passwordExpired>
        <sandbox>false</sandbox>
        <serverUrl>https://tamu-dev-ed.my.salesforce.com/services/Soap/m/39.0/xxxxxxxxxxxxxxxx</serverUrl>
        <sessionId>TEST-SESSION-ID-XXXXXXXXXX</sessionId>
        <userId>TEST-USER-ID-YYYYYYYYYY</userId>
        <userInfo>
        	<accessibilityMode>false</accessibilityMode>
        	<currencySymbol>￥</currencySymbol>
        	<orgAttachmentFileSizeLimit>5242880</orgAttachmentFileSizeLimit>
        	<orgDefaultCurrencyIsoCode>JPY</orgDefaultCurrencyIsoCode>
        	<orgDefaultCurrencyLocale>ja_JP</orgDefaultCurrencyLocale>
        	<orgDisallowHtmlAttachments>false</orgDisallowHtmlAttachments>
        	<orgHasPersonAccounts>false</orgHasPersonAccounts>
        	<organizationId>XXXXXXXXXXXXXXXXXXXXXX</organizationId>
        	<organizationMultiCurrency>false</organizationMultiCurrency>
        	<organizationName>XXXXXXXXXXXXXXXXXX</organizationName>
        	<profileId>XXXXXXXXXXXXXXXXXXX</profileId>
        	<roleId>XXXXXXXXXXXXXXXXXXXX</roleId>
        	<sessionSecondsValid>43200</sessionSecondsValid>
        	<userDefaultCurrencyIsoCode xsi:nil=\"true\"/>
        	<userEmail>tamura.shingo@gmail.com</userEmail>
        	<userFullName>Tamura Shingo</userFullName>
        	<userId>XXXXXXXXXXXXXXXXXX</userId>
        	<userLanguage>ja</userLanguage>
        	<userLocale>ja_JP</userLocale>
        	<userName>tamura.shingo@gmail.com</userName>
        	<userTimeZone>Asia/Tokyo</userTimeZone>
        	<userType>Standard</userType>
        	<userUiSkin>Theme3</userUiSkin>
        </userInfo>
      </result>
    </loginResponse>
  </soapenv:Body>
</soapenv:Envelope>"))
      (let* ((connection (cl-salesforce-bulk.connection:login "tamura.shingo@gmail.com" "password" "39.0"))
             (params (nth-mock-args-for 1 'dex:post)))
        (ok (string= "https://login.salesforce.com/services/Soap/u/39.0" (car params)))
        (not (null connection))
        (ok (string= "TEST-SESSION-ID-XXXXXXXXXX" (cl-salesforce-bulk.connection::session-id connection)))
        (ok (string= "39.0" (cl-salesforce-bulk.connection::api-version connection)))
        (ok (string= "tamu-dev-ed.my.salesforce.com" (cl-salesforce-bulk.connection::instance-host connection)))))))

(deftest post-data
  (testing "checking request uri"
    (with-dynamic-stubs ((dex:post 'ok))
      (let* ((connection (make-instance '<salesforce-connection>
                                        :session-id "12345678"
                                        :api-version "39.0"
                                        :instance-host "cl-salesforce-bulk.salesforce.com"))
             (result (cl-salesforce-bulk.connection:post-data connection "job" "data" '(("Content-Type" . "text/plain")
                                                                                        ("X-HEADER" . "test"))))
             (params (nth-mock-args-for 1 'dex:post)))
        (declare (ignore result))
        (ok (string= "https://cl-salesforce-bulk.salesforce.com/services/async/39.0/job" (car params)))
        (ok (equal '("X-SFDC-Session" . "12345678")
                   (assoc "X-SFDC-Session" (getf (cdr params) :headers) :test #'string=)))
        (ok (equal '("Content-Type" . "text/plain")
                   (assoc "Content-Type" (getf (cdr params) :headers) :test #'string=)))
        (ok (equal '("X-HEADER" . "test")
                   (assoc "X-HEADER" (getf (cdr params) :headers) :test #'string=)))
        (ok (string= "data"
                     (getf (cdr params) :content)))))))
