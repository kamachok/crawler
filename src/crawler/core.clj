(ns crawler.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojurewerkz.urly.core :as urly]))

(defn make-tree
  ([urls depth]
    (assoc (make-tree "process urls:" nil urls {} (- depth)) :type "root"))
  ([url root urls info depth]
    {:type "child" :root root :url url :urls urls :children (atom []) :info info :depth depth}))

(defn get-indent [count]
  (->> "   "
       (repeat )
       (take count)
       (str/join)))

(defn print-tree
  ([root]
    (print-tree 0 root))
  ([level root]
    (let [indent (get-indent level)
          url (:url root)
          children @(:children root)]
      (println indent url)
      (dorun (map (partial print-tree (inc level)) children)))))

(defn process-url [url depth]
  {:urls [(str url depth) (str url depth)] :info {}})

(defn add-child [root child]
  (swap! (:children root) conj child))

(defn travel-across-urls
  ([urls depth]
    (let [root (make-tree urls depth)]
      (travel-across-urls root)))
  ([root]
    (let [depth (:depth root)
          urls (:urls root)]
      (if (<= depth 0)
        (pmap (fn [url]
                (let [result (process-url url depth)
                      urls (:urls result)
                      info (:info result)
                      child (make-tree url root urls info (inc depth))]
                  (add-child root child)
                  (travel-across-urls child))) urls)) root)))

(defn is-valid-absolute-url? [line]
  (urly/absolute? line))

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
  (let [file-path "./samples/links.txt"
        depth (parse-int depth)]
    (if (is-valid-arguments file-path depth)
      (try
        (print-tree (travel-across-urls (read-urls-from-file file-path) depth))
        (finally (shutdown-agents))))))