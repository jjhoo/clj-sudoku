(ns clj-sudoku.core-test
  (:require [clojure.test :refer :all]
            [clj-sudoku.core :refer :all]))

(deftest solve-grid1
  (testing "solve grid1"
    (let [grids "200068050008002000560004801000000530400000002097000000804300096000800300030490007"
          [grid candidates] (init-solver grids)
          [ngrid ncandidates] (run-solver grid candidates)
          left (count ncandidates)]
      (is (= left 52)))))

(deftest solve-grid2
  (testing "solve grid2"
    (let [grids "000040700500780020070002006810007900460000051009600078900800010080064009002050000"
          [grid candidates] (init-solver grids)
          [ngrid ncandidates] (run-solver grid candidates)
          left (count ncandidates)]
      (is (= left 67)))))
