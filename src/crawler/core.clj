(ns crawler.core
  (:import (org.apache.http.conn ConnectTimeoutException)
           (java.net UnknownHostException))
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojurewerkz.urly.core :as urly]
            [clj-http.client :as client]
            [net.cgrand.enlive-html :as html]))

(def processed (atom #{}))

(defn make-tree
  ([urls depth]
    (assoc (make-tree "process urls:" nil urls {} (- depth)) :type "root"))
  ([url root urls info depth]
    {:type "child" :root root :url url :urls urls :children (atom []) :info info :depth depth}))

(defn get-indent [count]
  (->> "   "
       (repeat)
       (take count)
       (str/join)))

(defn print-tree
  ([root]
    (print-tree 0 root))
  ([level root]
    (let [indent (get-indent level)
          url (:url root)
          info (:info root)
          label (:label info)
          children @(:children root)]
      (println indent url label)
      (dorun (map (partial print-tree (inc level)) children)))))

(defn is-good-status? [status]
  (contains? #{200 201 202 203 204 205 206 207 300} status))

(defn is-redirect-status? [status]
  (contains? #{301 302 303 307} status))

(defn is-already-visited? [url]
  (contains? @processed url))

(defn is-valid-absolute-url? [line]
  (urly/absolute? line))

(defn is-host-changed? [root-url url]
  (if (not= root-url "process urls:")
    (let [host (.getHost (urly/url-like url))
          parent-host (.getHost (urly/url-like root-url))]
      (not= host parent-host))
    false))

(defn is-norm-link-href? [href]
  (if (nil? href) false (nil? (re-find (re-matcher #"^((javascript|mailto|callto):|#)" href)))))

(defn- normalize-urls [base-url href]
  (let [uri-href (urly/url-like href)]
    (.toString (if (not (urly/absolute? uri-href))
                 (urly/resolve (urly/url-like base-url) uri-href)
                 uri-href))))

(defn get-links-from-content [base-url content]
  (let [snippet (html/html-snippet content)
        elements (html/select snippet #{[:a]})]
    (reduce (fn [memo link]
              (let [href (:href (:attrs link))]
                (if (is-norm-link-href? href)
                  (conj memo (normalize-urls base-url href)))))
            [] elements)))

(defn http-get [url]
  (client/get url {:throw-exceptions false
                   :conn-timeout     2000
                   :follow-redirects false}))

(defn get-raw-responce [url]
  (try (http-get url)
       (catch ConnectTimeoutException e {:status 408})
       (catch UnknownHostException e {:status 404})
       (catch Exception e {:status 500})))

(defn wrap-info [to info]
  (assoc to :info info))

(defn get-page-info [root-url url]
  (cond
    (is-host-changed? root-url url) (wrap-info {:label "changed host"} {})
    (is-already-visited? url) (wrap-info {:label "already visided"} {})
    :else (do (swap! processed conj url)
              (let [raw (get-raw-responce url)
                    status (:status raw)
                    headers (:headers raw)]
                (cond
                  (is-good-status? status) (wrap-info {:label "ok"} {:content (:body raw)})
                  (is-redirect-status? status) (wrap-info {:label "redirect"} {:location (:location headers)})
                  :else (wrap-info {:label "bad link"} {:status status}))))))

(defn process-url [root-url url]
  (let [data (get-page-info root-url url)
        info (:info data)
        label (:label data)]
    (if (= label "ok")
      (let [content (:content info)
            urls (get-links-from-content root-url content)]
        (assoc data :urls urls))
      (assoc data :urls []))))

(defn add-child [root child]
  (swap! (:children root) conj child))

(defn travel-across-urls
  ([urls depth]
    (let [root (make-tree urls depth)]
      (travel-across-urls root)))
  ([root]
    (let [depth (:depth root)
          root-url (:url root)
          urls (:urls root)]
      (if (<= depth 0)
        (dorun (pmap #(let [result (process-url root-url %1)
                            urls (:urls result)
                            label (:label result)
                            info (:info result)
                            child (make-tree %1 root urls (assoc info :label label) (inc depth))]
                       (println result)
                       (add-child root child)
                       (travel-across-urls child)) urls)))
      root)))

(defn read-urls-from-file [file-path]
  (-> (slurp file-path)
      (str/split #"\n")
      (->> (reduce #(let [url (str/trim %2)] (if (is-valid-absolute-url? url) (conj %1 url) %1)) []))))

(defn parse-int [value]
  (try (if (string? value) (-> value Integer/parseInt)
                           value)
       (catch NumberFormatException e nil)))

(defn is-file-exist [file-path]
  (.exists (io/file file-path)))

(defn check-and-print [check message]
  (if (not check)
    (println message))
  check)

(defn is-valid-arguments [file-path depth]
  (and
    (check-and-print (is-file-exist file-path) (str "File with path: " file-path " doesn't exist."))
    (check-and-print (some? depth) "Depth must be a number")
    (check-and-print (>= depth 0) "Depth must be >= 0")))

(defn -main [file-path depth]
  (let [depth (parse-int depth)]
    (if (is-valid-arguments file-path depth)
      (try
        (print-tree (travel-across-urls (read-urls-from-file file-path) depth))
        (finally (shutdown-agents))))))