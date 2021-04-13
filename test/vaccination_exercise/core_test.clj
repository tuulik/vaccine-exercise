(ns vaccination-exercise.core-test
  (:require [clojure.test :refer :all]
            [vaccination-exercise.core :refer :all]
            [vaccination-exercise.name :as name]
            [tick.core :as t])
  (:import (java.util UUID)))

(deftest random-district-test
  (testing "Should return HYKS for value 0"
    (with-redefs [rand-int (constantly 0)]
      (is (= :HYKS (random-district)))))
  (testing "Should return :HYKS for value 2173797"
    (with-redefs [rand-int (constantly 2173797)]
      (is (= :HYKS (random-district)))))
  (testing "Should return :KYS for value 2173798"
    (with-redefs [rand-int (constantly 2173798)]
      (is (= :KYS (random-district)))))
  (testing "Should return :TYKS for maximun value 2173798"
    (with-redefs [rand-int (constantly (dec (reduce + (map :population districts))))]
      (is (= :TYKS (random-district))))))

(deftest random-bottle-test
  (testing "Should return order"
    (let [now (t/now)
          uuid-value (UUID/randomUUID)]
      (with-redefs [uuid (constantly uuid-value)
                    random-district (constantly :HYKS)
                    random-vaccine (constantly {:name       "Zerpfy"
                                                :injections 5})
                    random-arrival-since (constantly now)
                    name/random-name (constantly "Juho Friman")]
        (is (= (random-bottle 1)
               {:id                 uuid-value
                :orderNumber       1
                :responsiblePerson  "Juho Friman"
                :healthCareDistrict :HYKS
                :vaccine            "Zerpfy"
                :injections         5
                :arrived            now}))))))

(deftest random-vaccinationDate-test
  (let [now (t/now)
        injection {:id      "uuid"
                   :source  "uuid"
                   :arrived now
                   :vaccine "Zerpfy"}]
    (testing "Should return the date when order arrived if random element is minimum"
      (with-redefs [t/now (constantly now)
                    rand-int (constantly 0)]
        (is (= now (random-vaccinationDate injection)))))))
