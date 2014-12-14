(ns crawler.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-file-to-points [file-path]
  (-> (slurp file-path)
      (str/split #"\n")
      (->> (reduce
             (fn [memo line]s
               (conj memo (->> (str/split line #",")
                               (butlast)
                               (map #(Double/parseDouble (str/trim %1)))
                               (hash-map :values)))) []))))

(defn parse-int [value]
  (try (if (string? value) (-> value Integer/parseInt)
                           (value))
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
      (println "Hello crowler"))))