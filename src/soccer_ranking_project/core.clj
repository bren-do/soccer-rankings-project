(ns soccer-ranking-project.core
  (:require [clojure.java.io :as io]
            [clojure.tools.cli :as cli]
            [clojure.string :as string]
            [clojure.spec.alpha :as s])
  (:gen-class))

(def cli-options
  [["-f" "--filename FILENAME" "Filename"
    :default "sample-input.txt"]])

(defn game-result-string->game-result-tuple
  "Parses game-result string into a game-result tuple containing a team's name and score"
  [game-result-string]
  (->> (string/split game-result-string #", ")
       ;; Split game results into two team-score strings
       (map (fn [team-score] (string/split team-score #" ")))
       ;; Split each team-score string into constituent parts
       (mapv (fn [team-score-atoms]
              {:name (string/join " " (drop-last team-score-atoms))
               :score (read-string (last team-score-atoms))}))
       ;; For each team-score consituent parts,
       ;; Create map with name and score fields
       ;; Return for each game a tuple of maps containing name and score for each team
       ))

(defn input->game-results
  "Takes valid input string of game results for a league and returns a seq of tuples of game-result"
  [input]
  (->> (string/split-lines input)
       (map game-result-string->game-result-tuple)))

(defn calculate-points-gained-by-team-in-game
  "Given a game-result tuple will return a map containing the league points gained by each team"
  [[team-a team-b]]
  (let [score-a (:score team-a)
        score-b (:score team-b)
        name-a (:name team-a)
        name-b (:name team-b)]
    (cond
      (< score-a score-b) {name-a 0
                           name-b 3}
      (= score-a score-b) {name-a 1
                           name-b 1}
      :else {name-a 3
             name-b 0})))

(defn generate-points-by-team
  "Given a seq of game-result tuples will generate a map of total league points for each team"
  [results]
  (reduce (fn [coll result]
            (merge-with +
                        coll
                        (calculate-points-gained-by-team-in-game result))) {} results))

(defn generate-sort-order-rankings
  "Takes a sorted team-points seq and returns an ordered seq of ranking maps based on sort order alone."
  [sorted-team-points]
  (map (fn [rank [name points]]
         {:rank (inc rank)
          :name name
          :points points}) (range (count sorted-team-points)) sorted-team-points))

(defn generate-true-rankings
  "Takes an ordered seq of ranking maps based on sort order and returns a true ordered seq of ranking maps where teams that tie are given the same ranking"
  [sort-order-ranking]
  (loop [[team-a team-b & rest] sort-order-ranking output []]
    (if team-b
      (let [team-a-points (:points team-a)
            team-b-points (:points team-b)
            team-a-rank (:rank team-a)
            updated-team-b (cond
                             (= team-a-points team-b-points) (assoc team-b :rank team-a-rank)
                             :else team-b)]
        (recur (cons updated-team-b rest) (conj output team-a)))
      (conj output team-a))))

(defn generate-rankings
  "Given a map of points-by-team will generate a seq of ranking maps ordered from best to worst rank"
  [points-by-team]
  (let [sorted-team-points (->> (sort-by second points-by-team)
                                reverse)
        sort-order-rankings (generate-sort-order-rankings sorted-team-points)]
    (generate-true-rankings sort-order-rankings)))


(defn print-rankings!
  "Prints rankings"
  [rankings]
  (run! (fn [{:keys [rank name points]}]
          (cond
            (not= 1 points) (println (format "%s. %s, %s pts" rank name points))
            :else (println (format "%s. %s, %s pt" rank name points)))) rankings))


(defn -main
  [& args]
  (let [opts (cli/parse-opts args cli-options)
        filename (get-in opts [:options :filename])]
    (when-let [input (slurp (io/file filename))]
      (let [game-results (input->game-results input)
            points-by-team (generate-points-by-team game-results)
            rankings (generate-rankings points-by-team)]
        (print-rankings! rankings)))))
