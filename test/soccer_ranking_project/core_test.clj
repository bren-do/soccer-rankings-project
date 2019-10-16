(ns soccer-ranking-project.core-test
  (:require [clojure.test :refer :all]
            [soccer-ranking-project.core :as core]))

(def input-expected-output-tuples
  "Sequence of test case tuples
  left is input
  right is expected output"
  [["Lions 3, Snakes 3" [{:name "Lions" :score 3}
                         {:name "Snakes" :score 3}]]
   ["Tarantulas 1, FC Awesome 0" [{:name "Tarantulas" :score 1}
                                  {:name "FC Awesome" :score 0}]]
   ["Lions 1, FC Awesome 1" [{:name "Lions" :score 1}
                             {:name "FC Awesome" :score 1}]]
   ["Tarantulas 3, Snakes 1" [{:name "Tarantulas" :score 3}
                              {:name "Snakes" :score 1}]]
   ["Lions 4, Grouches 0" [{:name "Lions" :score 4}
                           {:name "Grouches" :score 0}]]])

(deftest result-string->result-tuple-test
  (testing "Ensures valid input returns expected result-tuple"
    (doseq [[input expected-output] input-expected-output-tuples]
      (is (= (core/game-result-string->game-result-tuple input)
             expected-output)))))

(deftest generate-points-by-team-test
  (testing "Ensures game-result tuples will generate a map of league points by team"
    (let [expected-output {"Lions" 5, "Snakes" 1, "Tarantulas" 6, "FC Awesome" 1, "Grouches" 0}
          input '([{:name "Lions", :score 3} {:name "Snakes", :score 3}]
                 [{:name "Tarantulas", :score 1} {:name "FC Awesome", :score 0}]
                 [{:name "Lions", :score 1} {:name "FC Awesome", :score 1}]
                 [{:name "Tarantulas", :score 3} {:name "Snakes", :score 1}]
                 [{:name "Lions", :score 4} {:name "Grouches", :score 0}])]
      (is (= (core/generate-points-by-team input) expected-output)))))

(deftest generate-rankings-test
  (testing "Ensures points-by-team map based on sample input will generate correct rankings"
    (let [input {"Lions" 5, "Snakes" 1, "Tarantulas" 6, "FC Awesome" 1, "Grouches" 0}
          expected-output [{:rank 1, :name "Tarantulas", :points 6}
                           {:rank 2, :name "Lions", :points 5}
                           {:rank 3, :name "FC Awesome", :points 1}
                           {:rank 3, :name "Snakes", :points 1}
                           {:rank 5, :name "Grouches", :points 0}]]
      (is (= (core/generate-rankings input) expected-output))))
  (testing "Ensures points-by-team map where every team is tied will generate correct rankings"
    (let [input {"Lions" 5, "Snakes" 5, "Tarantulas" 5, "FC Awesome" 5, "Grouches" 5}
          expected-output [{:rank 1, :name "Grouches", :points 5}
                           {:rank 1, :name "FC Awesome", :points 5}
                           {:rank 1, :name "Tarantulas", :points 5}
                           {:rank 1, :name "Snakes", :points 5}
                           {:rank 1, :name "Lions", :points 5}]]
      (is (= (core/generate-rankings input) expected-output))))
  (testing "Ensures points-by-team map where every team is not tied will generate correct rankings"
    (let [input {"Lions" 5, "Snakes" 1, "Tarantulas" 6, "FC Awesome" 2, "Grouches" 0}
          expected-output [{:rank 1, :name "Tarantulas", :points 6}
                           {:rank 2, :name "Lions", :points 5}
                           {:rank 3, :name "FC Awesome", :points 2}
                           {:rank 4, :name "Snakes", :points 1}
                           {:rank 5, :name "Grouches", :points 0}]]
      (is (= (core/generate-rankings input) expected-output)))))
