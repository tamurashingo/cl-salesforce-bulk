(defpackage cl-salesforce-bulk/tests/connection
  (:use :cl
        :cl-salesforce-bulk.connection
        :mockingbird
        :rove))
(in-package :cl-salesforce-bulk/tests/connection)



(deftest login
  (testing "response check"
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
    (let ((connection (cl-salesforce-bulk.connection:login "tamura.shingo@gmail.com" "password" "39.0")))
      (ok (= 1 1))
      (not (null connection))
      (ok (string= "TEST-SESSION-ID-XXXXXXXXXX" (cl-salesforce-bulk.connection::session-id connection)))
      (ok (string= "39.0" (cl-salesforce-bulk.connection::api-version connection)))
      (ok (string= "tamu-dev-ed.my.salesforce.com" (cl-salesforce-bulk.connection::instance-host connection)))))))


