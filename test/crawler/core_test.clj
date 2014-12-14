(ns crawler.core-test
  (:import (java.net UnknownHostException)
           (org.apache.http.conn ConnectTimeoutException))
  (:require [clojure.test :refer :all]
            [crawler.core :refer :all :as core]))

(def test-url "http://crawler.com/")
(def test-url-404 (str test-url 404))
(def test-url-500 (str test-url 500))

(deftest test-get-links-from-content
  (testing "Test find absolute links and relative links"
    (let [html (str "<div><a href=\"http://crawler.com/somepath\"></a><a href=\"https://crawler.com/somepath2\"></a>"
                    "<a href=\"other.com\"></a><a href=\"/part\"></a><a href=\"part?asdsad=true\"></a></div>")
          links (core/get-links-from-content test-url html)]
      (is (= (count links) 5))))
  (testing "Test is parse skip bad links like (anchors - # and other protocols - javascript|mailto|callto) "
    (let [html (str "<div><a href=\"http://crawler.com/somepath\"></a><a href=\"#part\"></a><a href=\"#another/test\"></a>"
                    "<a href=\"javascript: alert(\"asdasdsa\")\"></a><a href=\"mailto:test@crawler.com\"></a><a href=\"callto:test@crawler.com\"></a></div>")
          links (core/get-links-from-content test-url html)]
      (is (= (count links) 1)))))


(defn- get-request-wrap
  [status]
  (case status
    test-url-404 (throw (UnknownHostException. "404"))
    test-url-500 (throw (Exception. "500"))))

(defn wrap-setup
  [test-fn]
  (with-redefs-fn {#'core/http-get get-request-wrap} test-fn))

(use-fixtures :each wrap-setup)

(deftest handle-errors
  (testing "Check 404 eror handling"
    (let [raw-restonse (#'core/get-page-info test-url test-url-404)
          label (:label raw-restonse)]
      (is (= label "bad link"))))
  (testing "Check 500 eror handling"
    (let [raw-restonse (#'core/get-page-info test-url test-url-500)
          label (:label raw-restonse)]
      (is (= label "bad link")))))