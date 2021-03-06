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

(deftest solve-grid-hidden-triple
  (testing "solve grid with hidden triple"
    (let [grids "300000000970010000600583000200000900500621003008000005000435002000090056000000001"
          [grid candidates] (init-solver grids)
          [ngrid ncandidates] (run-solver grid candidates)
          left (count ncandidates)]
      (is (= left 0)))))

(deftest solve-grid-pointing-pairs
  (testing "solve grid with pointing pairs"
    (let [grids "000000000904607000076804100309701080008000300050308702007502610000403208000000000"
          [grid candidates] (init-solver grids)
          [ngrid ncandidates] (run-solver grid candidates)
          left (count ncandidates)]
      (is (= left 0)))))
