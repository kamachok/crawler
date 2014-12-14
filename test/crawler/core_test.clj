(ns crawler.core-test
  (:require [clojure.test :refer :all]
            [crawler.core :refer :all :as core]))

(def test-url "http://crawler.com")

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
      (println links)
      (is (= (count links) 1)))))