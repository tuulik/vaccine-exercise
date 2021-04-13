(ns vaccination-exercise.name)

(def first-name
  (delay (-> "resources/first_names.txt"
             slurp
             clojure.string/split-lines)))

(def last-names
  (delay (-> "resources/last_names.txt"
             slurp
             clojure.string/split-lines)))

(defn random-name
  []
  (str (nth @first-name (rand-int (count @first-name))) " " (nth @last-names (rand-int (count @last-names)))))

(defn random-gender
  []
  (nth ["male" "female" "nonbinary"] (rand-int 3)))
