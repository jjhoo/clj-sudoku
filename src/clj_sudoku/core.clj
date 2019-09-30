;; Copyright (c) 2019 Jani J. Hakala <jjhakala@gmail.com> Jyväskylä, Finland
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as
;;  published by the Free Software Foundation, version 3 of the
;;  License.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Affero General Public License for more details.
;;
;;  You should have received a copy of the GNU Affero General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
(ns clj-sudoku.core
  (:gen-class)
  (:require [clojure.set]
            [clojure.math.combinatorics]))

(defprotocol Filtering
  (in-box [this box])
  (in-column [this column])
  (in-row [this row])
  (in-same-box [this other])
  (in-same-column [this other])
  (in-same-row [this other]))

(defprotocol CellFiltering
  (in-same-unit [this other]))

(defrecord Box [^byte row ^byte column]
  Object
  (toString [_]
    (str "(" row " " column ")")))

(defrecord Pos [^byte row ^byte column ^Box box]
  Filtering
  (in-box [this obox] (= box obox))
  (in-column [this ocolumn] (= column ocolumn))
  (in-row [this orow] (= row orow))
  (in-same-box [this other] (= box (:box other)))
  (in-same-column [this other] (= (:column this) (:column other)))
  (in-same-row [this other] (= (:row this) (:row other)))

  Object
  (toString [_]
    (str "(" row " " column " " box ")"))

  Comparable
  (compareTo [this other]
    (let [rcmp (compare row (:row other))]
      (if (not (zero? rcmp))
        rcmp
        (compare column (:value column))))))

(defrecord Cell [^byte value ^Pos pos]
  CellFiltering
  (in-same-unit [this other]
    (or (.in-same-column pos (:pos other))
        (.in-same-row pos (:pos other))
        (.in-same-box pos (:pos other))))

  Comparable
  (compareTo [this other]
    (let [cmp (compare pos (:pos other))]
      (if (not (zero? cmp))
        cmp
        (compare value (:value other))))))

(defmethod print-method Box [^Box v ^java.io.Writer w]
  (.write w (str "Box#{row: " (.row v) ", column: " (.column v) "}")))

(defmethod print-method Pos [^Pos v ^java.io.Writer w]
  (.write w (str "Pos#{row: " (.row v) ", column: " (.column v) "}")))

(defmethod print-method Cell [^Cell v ^java.io.Writer w]
  (.write w (str "Cell#{value: " (.value v) ", pos: " (str (.pos v)) "}")))

(defn make-box
  [row column]
  (->Box (inc (/ (dec row) 3))
         (inc (/ (dec column) 3))))

(defn make-pos
  [row column]
  (->Pos row column (make-box row column)))

(defn pos-to-vector-index
  [^Pos pos]
  (+ (* 9 (dec (.row pos))) (dec (.column pos))))

(defn make-cell
  [value row column]
  (->Cell value (make-pos row column)))

(defn print-grid
  [grid]
  (println "+-------------------+")
  (loop [i 0
         grid grid]
    (if (empty? grid)
      (do
        (println "+-------------------+")
        nil)
      (let [^Cell item (first grid)
            v (.value item)]
        (if (= (rem (inc i) 9) 1)
          (print "| "))
        (if (= v 0)
          (print ".")
          (print v))
        (if (= (rem (inc i) 9) 0)
          (println " |")
          (print " "))
        (recur (inc i) (rest grid))))))

(defn str-to-grid
  [grid]
  (into [] (map-indexed (fn [i c]
                          (let [v (- (byte c) (byte \0))
                                row (inc (/ i 9))
                                col (inc (rem i 9))]
                            (make-cell v row col)))
                        (seq grid))))

(defn init-candidates
  [grid]
  (let [nexti (fn [i j n]
                (if (= n 9)
                  (cond
                    (and (= i 9) (= j 9))     nil
                    (= j 9)                   [(inc i) 1 1]
                    :else                     [i (inc j) 1])
                  [i j (inc n)]))
        cands (fn loopy
                ([] (apply loopy [1 1 1]))
                ([i j n]
                 (let [nstate (nexti i j n)]
                   (cond (nil? nstate)
                         (lazy-seq (cons (make-cell n i j) nil))
                         :else (lazy-seq (cons (make-cell n i j)
                                               (apply loopy nstate)))))))]
    (cands)))

(defn grid-to-string
  [grid]
  (let [sb (new StringBuilder)
        byte-to-char (fn [byteval] (char (+ (byte \0) byteval)))]
    (doseq [^Cell cell grid]
      (.append sb (byte-to-char (.value cell))))
    (.toString sb)))

(defn grid-update-solved
  [grid solved]
  (let [solved-map (into {} (map (fn [^Cell cell] [(.pos cell) cell]) solved))]
    (reduce (fn [acc [pos v]]
              (assoc acc (pos-to-vector-index pos) v))
            grid
            solved-map)))

(defn candidates-remove-solutions
  [grid cands]
  ;; (println "candidates-remove-solutions")
  (loop [ngrid grid ncands cands]
    (if (empty? ngrid)
      ncands
      (let [^Cell head (first ngrid)
            tail (rest ngrid)
            pos (.pos head)
            value (.value head)
            nncands (filter (fn [^Cell cell]
                              (not (or (= pos (.pos cell))
                                       (and (= value (.value cell))
                                            (.in-same-unit head cell)))))
                            ncands)]
        (recur tail nncands)))))

(defn number-to-box
  [^Long box]
  (->Box (inc (/ (dec box) 3))
         (inc (rem (dec box) 3))))

(defn get-cells-box-n
  [cells ^Byte box]
  (filter (fn [^Cell cell]
            (and (not= (:value cell) 0)
                 (in-box (:pos cell) (number-to-box box)))) cells))

(defn get-cells-box
  [cells ^Box box]
  (filter (fn [^Cell cell]
            (and (not= (:value cell) 0)
                 (in-box (:pos cell) box))) cells))

(defn get-cells-column
  [cells ^Byte column]
  (filter (fn [^Cell cell]
            (and (not= (:value cell) 0)
                 (in-column (:pos cell) column))) cells))

(defn get-cells-row
  [cells ^Byte row]
  (filter (fn [^Cell cell]
            (and (not= (:value cell) 0)
                 (in-row (:pos cell) row))) cells))

(defn ucpos
  [cells]
  (distinct (map (fn [^Cell cell] (.pos cell)) cells)))

(deftype FinderResult [solved eliminated])

(defn finder
  [pred cands & {:keys [noboxes] :or {noboxes false}}]
  (loop [seq (for [i (range 1 10)
                   fun (if noboxes
                         [get-cells-row get-cells-column]
                         [get-cells-row get-cells-column get-cells-box-n])]
               [i fun])
         solved []
         eliminated []]
    (if (empty? seq)
      (->FinderResult (distinct solved) (distinct eliminated))
      (let [[i fun] (first seq)
            cells (fun cands i)]
        (if
         (empty? cells)
          (recur (rest seq) solved eliminated)
          (let [^FinderResult fr (pred cells)]
            (recur (rest seq)
                   (concat solved (.solved fr))
                   (concat eliminated (.eliminated fr)))))))))

(defn find-singles-simple
  [cands]
  (let [grouped (group-by :pos cands)
        singled (map (comp first second)
                     (filter (fn [[_ cells]]
                               (== (count cells) 1))
                             grouped))]
    (->FinderResult singled [])))

(defn find-singles
  [cands]
  (let [fun (fn [cells]
              (let [grouped (group-by :value cells)
                    singled (map (comp first second)
                                 (filter (fn [[_ cells]]
                                           (== (count cells) 1))
                                         grouped))]
                (->FinderResult singled [])))]
    (finder fun cands)))

(defn find-naked-sets
  [cands n]
  (let [fun (fn [cells]
              (let [ns (set (map :value cells))
                    combs (map set (clojure.math.combinatorics/combinations ns n))
                    pos-cells (map (fn [[pos cells]]
                                     [pos (set (map :value cells))])
                                   (group-by :pos cells))]
                (loop [combs combs
                       found []]
                  (if (empty? combs)
                    (->FinderResult [] found)
                    (let [comb (first combs)
                          pos-numbers (filter (fn [[pos values]]
                                                (clojure.set/superset? comb values))
                                              pos-cells)]
                      (if (= (count pos-numbers) n)
                        (let [pair (set (map first pos-numbers))
                              others (filter (fn [^Cell cell]
                                               (and (not (contains? pair (:pos cell)))
                                                    (contains? comb (:value cell))))
                                             cells)]
                          (if (empty? others)
                            (recur (rest combs) found)
                            (recur (rest combs) (concat found others))))
                        (recur (rest combs) found)))))))]
    (finder fun cands)))

(defn find-naked-pairs
  [cands]
  (find-naked-sets cands 2))

(defn find-naked-triples
  [cands]
  (find-naked-sets cands 3))

(defn find-naked-quads
  [cands]
  (find-naked-sets cands 4))

(defn find-boxline-reductions
  [cands]
  (let [fun (fn [cells]
              (let [n-grouped (group-by :value cells)
                    box-line-cells (filter (fn [[_ cells]]
                                             (if (= (count cells) 1)
                                               false
                                               (let [^Cell a (first cells)]
                                                 (every? (fn [^Cell b]
                                                           (.in-same-box ^Pos (:pos a)
                                                                         ^Pos (:pos b)))
                                                         (rest cells)))))
                                           n-grouped)]
                (loop [box-line-cells box-line-cells
                       found []]
                  (if (empty? box-line-cells)
                    (->FinderResult [] found)
                    (let [[n cells] (first box-line-cells)
                          pos (:pos (first cells))
                          same-box? (partial (fn [^Pos pos1 ^Pos pos2]
                                               (.in-same-box pos1 pos2))
                                             pos)
                          box-cells-n (filter #(= n (:value %1)) (get-cells-box cands (:box pos)))
                          all (set (map :pos box-cells-n))
                          exclude (set (map :pos cells))
                          diff (clojure.set/difference all exclude)
                          nfound (filter #(contains? diff (:pos %1)) box-cells-n)]
                      (if (empty? diff)
                        (recur (rest box-line-cells) found)
                        (recur (rest box-line-cells) (concat found nfound))))))))]
    (finder fun cands :noboxes true)))

(defn init-solver
  [^String str]
  (let [grid (str-to-grid str)
        cands (init-candidates grid)
        nonz (filter (fn [^Cell cell] (not (= (.value cell) 0))) grid)]
    [grid (candidates-remove-solutions nonz cands)]))

(defn run-solver
  [grid candidates]
  (let [finders [find-singles-simple
                 find-singles
                 find-naked-pairs
                 find-naked-triples
                 find-naked-quads
                 find-boxline-reductions]]
    (loop [grid grid
           candidates candidates
           nfinders finders]
      (cond (empty? nfinders)
            [grid candidates]
            ;;
            :else (let [finderFun (first nfinders)
                        ^FinderResult res (finderFun candidates)
                        solved (.solved res)
                        eliminated (.eliminated res)]
                    (cond
                      (not (empty? solved))
                      (let [ngrid (grid-update-solved grid solved)
                            ncandidates (candidates-remove-solutions solved candidates)]
                        (recur ngrid ncandidates finders))
                      ;;
                      (not (empty? eliminated))
                      (let [s (set eliminated)
                            ncandidates (remove #(contains? s %1) candidates)]
                        (recur grid ncandidates finders))
                      ;;
                      :else (recur grid candidates (rest nfinders))))))))


(gen-class
 :name "fi.koodisuo.clj-sudoku.Sudoku"
 :state state
 :init init
 :constructors {[String] []}
 :prefix "sudoku-"
 :methods [[solve [] void]])

(import 'fi.koodisuo.clj-sudoku.Sudoku)

(defn sudoku-init
  [^String str]
  (let [[grid candidates] (init-solver str)]
    [[] (ref {:solved grid :candidates candidates})]))

(defn sudoku-solve
  [^Sudoku this]
  (let [grid (:solved @(.state this))
        candidates (:candidates @(.state this))
        [ngrid ncandidates] (run-solver grid candidates)]
    (dosync (alter (.state this) assoc :solved ngrid :candidates ncandidates))))

(defn -main
  [& args]
  (let [grid "200068050008002000560004801000000530400000002097000000804300096000800300030490007"
        xgrid (str-to-grid grid)
        ^Pos pos1 (make-pos 1 1)
        ^Pos pos1b (make-pos 1 1)
        ^Pos pos2 (make-pos 1 3)
        ^Pos pos3 (make-pos 3 1)
        ^Pos pos4 (make-pos 5 5)]

    ;; (doseq [x (init-candidates xgrid)]
    ;; (println "candidate" x))

    ;; (doseq [item xgrid]
    ;;  (println item))
    (println (.in-same-row pos1 pos2)
             (.in-same-row pos1 pos3)
             (.in-same-row pos1 pos4))

    (println (.in-same-column pos1 pos2)
             (.in-same-column pos1 pos3)
             (.in-same-column pos1 pos4))

    (println (.in-same-box pos1 pos2)
             (.in-same-box pos1 pos3)
             (.in-same-box pos1 pos4))

    (println (= pos1 pos1b))

    (print-grid xgrid)
    (println (grid-to-string xgrid))
    (let [sudoku (Sudoku. grid)]
      ;; (doseq [c (:candidates (.state sudoku))]
      ;;  (println "  " c))
      (.solve ^Sudoku sudoku)
      (let [solved (:solved @(.state ^Sudoku sudoku))]
        (print-grid solved)
        (println (grid-to-string solved)))
      (println "candidates left" (count (:candidates @(.state ^Sudoku sudoku)))))))
