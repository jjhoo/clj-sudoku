(ns clj-sudoku.nsclass-test
  (:require [clojure.test :refer :all])
  (:import (fi.koodisuo.clj-sudoku Sudoku)))

(deftest solve-grid1
  (testing "solve grid1"
    (let [grid "200068050008002000560004801000000530400000002097000000804300096000800300030490007"
          sudoku (Sudoku. grid)]
      (.solve ^Sudoku sudoku)
      (let [solved (:solved @(.state ^Sudoku sudoku))
            left (count (:candidates @(.state ^Sudoku sudoku)))]
        (is (= left 52))))))

(deftest solve-grid2
  (testing "solve grid2"
    (let [grid "000040700500780020070002006810007900460000051009600078900800010080064009002050000"
          sudoku (Sudoku. grid)]
      (.solve ^Sudoku sudoku)
      (let [solved (:solved @(.state ^Sudoku sudoku))
            left (count (:candidates @(.state ^Sudoku sudoku)))]
        (is (= left 67))))))
