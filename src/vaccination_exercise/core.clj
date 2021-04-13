(ns vaccination-exercise.core
  (:require [tick.alpha.api :as t]
            [vaccination-exercise.name :as name]
            [jsonista.core :as j])
  (:import (java.util UUID)))

(defn uuid
  [] (UUID/randomUUID))

(def districts [{:district-name :HYKS
                 :population    2173797}
                {:district-name :KYS
                 :population    805133}
                {:district-name :OYS
                 :population    738690}
                {:district-name :TAYS
                 :population    900724}
                {:district-name :TYKS
                 :population    869786}])

(defn random-district
  []
  (loop [d districts
         slot (rand-int (reduce + (map :population districts)))]
    (let [{:keys [district-name population]} (first d)]
      (if (<= (- slot population) 0)
        district-name
        (recur (rest d) (- slot population))))))

(def vaccines
  [{:name       "Zerpfy"
    :injections 5}
   {:name       "Antiqua"
    :injections 4}
   {:name       "SolarBuddhica"
    :injections 6}])

(defn random-vaccine
  []
  (first (shuffle vaccines)))

(defn random-seconds-between-days
  "Returns seconds between random instant from last days"
  [days]
  (rand-int (* days 24 60 60)))

(defn random-arrival-since
  [now-instant seconds-before-arrived]
  (t/- now-instant (t/new-duration seconds-before-arrived :seconds)))

(defn- generate-bottle
  "Returns vaccine bottle from last 100 days"
  [vaccine orderNumber arrived]
  {:id                 (uuid)
   :orderNumber       orderNumber
   :responsiblePerson  (name/random-name)
   :healthCareDistrict (random-district)
   :vaccine            (:name vaccine)
   :injections         (:injections vaccine)
   :arrived            arrived})

(defn random-bottle
  [orderNumber]
  (generate-bottle (random-vaccine) orderNumber (random-arrival-since (t/now) (random-seconds-between-days 100))))

(defn create-bottles
  [n]
  (mapv random-bottle (range n)))

(defn bottle-to-injections
  [bottle]
  (mapv (fn [_] {:source  (:id bottle)
                 :arrived (:arrived bottle)
                 :vaccine (:vaccine bottle)}) (range (:injections bottle))))

(defn bottles-to-injections
  [bottles]
  (flatten (map bottle-to-injections bottles)))

(defn random-vaccinationDate
  [injection]
  (let [now (t/now)
        expires (t/+ (:arrived injection) (t/new-duration 30 :days))
        expiration-or-now (if (t/< expires now) expires (t/now))
        vacc-date (t/- expiration-or-now (t/new-duration (rand-int (t/seconds (t/between (:arrived injection)
                                                                                         expiration-or-now))) :seconds))]
    (assert (t/<= vacc-date now))
    (assert (t/<= (:arrived injection) vacc-date expires))
    vacc-date))

(defn remove-injection
  [injections injection-to-remove]
  (vec (concat (subvec injections 0 injection-to-remove) (subvec injections (inc injection-to-remove)))))

(defn add-to-used-injections
  [used-injections injection vaccinationDate]
  (conj used-injections {:vaccination-id   (UUID/randomUUID)
                         :sourceBottle    (:source injection)
                         :gender           (name/random-gender)
                         :vaccinationDate vaccinationDate}))

(defn inject
  [injections iterations]
  (loop [iterations iterations
         injections-left injections
         used-injections []]
    (cond
      (or (zero? iterations) (zero? (count injections-left)))
      {:used used-injections :left injections-left}

      :else
      (let [injection-number (rand-int (count injections-left))
            injection (nth injections-left injection-number)
            vaccinationDate (random-vaccinationDate injection)
            vaccinations-left (remove-injection injections-left injection-number)
            used-vaccinations (add-to-used-injections used-injections injection vaccinationDate)]
        (recur (dec iterations) vaccinations-left used-vaccinations)))))

(defn generate-source
  [bottles iterations]
  (let [bottles (vec (create-bottles bottles))
        result (inject (vec (bottles-to-injections bottles)) iterations)]
    (println "Generating producer files")
    (let [grouped (group-by :vaccine bottles)]
      (doseq [producer (keys grouped)]
        (println "Generating " producer)
        (doseq [order (get grouped producer)]
          (spit (str "resources/" producer ".source") (str (j/write-value-as-string order) "\n") :append true))))
    (println "Generating injection results")
    (doseq [injection (:used result)]
      (spit (str "resources/vaccinations.source") (str (j/write-value-as-string injection) "\n") :append true))))


