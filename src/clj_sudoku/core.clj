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
  (:gen-class))

(if *compile-files*
  (set! *warn-on-reflection* true))

(defprotocol Filtering
  (in-box [this box])
  (in-column [this column])
  (in-row [this row])
  (in-same-box [this other])
  (in-same-column [this other])
  (in-same-row [this other]))

(defprotocol CellFiltering
  (in-same-unit [this other]))

(deftype Box [^byte row ^byte column]
  Object
  (equals [this other]
    (and (= column (.column other))
         (= row (.row other))))
  (toString [_]
    (str "(" row " " column ")")))

(deftype Pos [^byte row ^byte column ^Box box]
  Filtering
  (in-box [this obox] (= box obox))
  (in-column [this ocolumn] (= column ocolumn))
  (in-row [this orow] (= row orow))
  (in-same-box [this other] (= box (.box other)))
  (in-same-column [this other] (= (.column this) (.column other)))
  (in-same-row [this other] (= (.row this) (.row other)))

  Object
  (equals [this other]
    (and (= column (.column other))
         (= row (.row other))))
  (toString [_]
    (str "(" row " " column " " box ")")))

(deftype Cell [^byte value ^Pos pos]
  CellFiltering
  (in-same-unit [this other]
    (or (.in-same-column pos (.pos other))
        (.in-same-row pos (.pos other))
        (.in-same-box pos (.pos other))))

  Object
  (equals [this other]
    (and (= value (.value other))
         (= pos (.pos other))))
  (toString [_]
    (str "Cell#{value: " value ", pos: " pos "}")))

(defmethod print-method Box [v ^java.io.Writer w]
  (.write w (str "Box#{row: " (.row v) ", column: " (.column v) "}")))

(defmethod print-method Pos [v ^java.io.Writer w]
  (.write w (str "Pos#{row: " (.row v) ", column: " (.column v) "}")))

(defmethod print-method Cell [v ^java.io.Writer w]
  (.write w (str "Cell#{value: " (.value v) ", pos: " (str (.pos v)) "}")))

(defn make-box
  [row column]
  (Box. (+ (/ (- row 1) 3) 1)
        (+ (/ (- column 1) 3) 1)))

(defn make-pos
  [row column]
  (Pos. row column (make-box row column)))

(defn pos-to-vector-index
  [^Pos pos]
  (+ (* 9 (- (.row pos) 1)) (- (.column pos) 1)))

(defn make-cell
  [value row column]
  (Cell. value (make-pos row column)))

(defn print-grid
  [grid]
  (println "+-------------------+")
  (loop [i 0
         grid grid]
    (if (empty? grid)
      (do
        (println "+-------------------+")
        nil)
      (let [item (first grid)
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
                                row (+ (/ i 9) 1)
                                col (+ (rem i 9) 1)]
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
    (doseq [cell grid]
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
      (let [head (first ngrid)
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
  (Box. (inc (/ (dec box) 3))
        (inc (rem (dec box) 3))))

(defn get-cells-box
  [^clojure.lang.ISeq cells ^Byte box]
  (filter (fn [^Cell cell]
            (and (not (= (.value cell) 0))
                 (.in-box (.pos cell) (number-to-box box)))) cells))

(defn get-cells-column
  [^clojure.lang.ISeq cells ^Byte column]
  (filter (fn [^Cell cell]
            (and (not (= (.value cell) 0))
                 (.in-column (.pos cell) column))) cells))

(defn get-cells-row
  [^clojure.lang.ISeq cells ^Byte row]
  (filter (fn [^Cell cell]
            (and (not (= (.value cell) 0))
                 (.in-row (.pos cell) row))) cells))

(defn ucpos
  [cells]
  (distinct (map (fn [^Cell cell] (.pos cell)) cells)))

(deftype FinderResult [^clojure.lang.ISeq solved ^clojure.lang.ISeq eliminated])

(defn finder
  [pred ^clojure.lang.ISeq cands]
  (loop [seq (for [i (range 1 10)
                   fun [get-cells-row get-cells-column get-cells-box]]
               [i fun])
         solved []
         eliminated []]
    (if (empty? seq)
      (FinderResult. (distinct solved) (distinct eliminated))
      (let [[i fun] (first seq)
            ^FinderResult fr (pred (fun cands i))]
        (recur (rest seq)
               (concat solved (.solved fr))
               (concat eliminated (.eliminated fr)))))))

(defn find-singles-simple
  [^clojure.lang.ISeq cands]
  (let [fun (fn [^clojure.lang.ISeq cells]
              (loop [poss (ucpos cells)
                     solved []]
                (if (empty? poss)
                  (FinderResult. solved [])
                  (let [head (first poss)
                        tail (rest poss)
                        samepos (filter (fn [^Cell cell]
                                          (= head (.pos cell)))
                                        cells)]
                    (if (= 1 (count samepos))
                      (recur tail (concat solved samepos))
                      (recur tail solved))))))]
    (finder fun cands)))

(gen-class
 :name clj-sudoku.core.Sudoku
 :state state
 :init init
 :constructors {[String] []}
 :prefix "sudoku-"
 :methods [[solve [] void]])

(import 'clj-sudoku.core.Sudoku)

(defn sudoku-init
  [^String str]
  (let [grid (str-to-grid str)
        cands (init-candidates grid)
        nonz (filter (fn [^Cell cell] (not (= (.value cell) 0))) grid)]
    [[] (ref {:solved grid :candidates (candidates-remove-solutions nonz cands)})]))

(defn sudoku-solve
  [^Sudoku this]
  (let [finders [find-singles-simple]]
    (loop [grid (:solved @(.state this))
           candidates (:candidates @(.state this))
           nfinders finders]
      (cond (empty? nfinders)
            (dosync
             (ref-set (.state this) (assoc @(.state this) :solved grid :candidates candidates)))
            ;;
            :else (let [finder (first nfinders)
                        res (finder candidates)
                        solved (.solved res)
                        eliminated (.eliminated res)]
                    (cond
                      (not (empty? solved))
                      (let [ngrid (grid-update-solved grid solved)
                            ncandidates (candidates-remove-solutions solved candidates)]
                        (recur ngrid ncandidates finders))
                      ;;
                      (not (empty? eliminated))
                      (do
                        (println "Update eliminated")
                        (recur grid candidates (rest nfinders)))
                      ;;
                      :else (recur grid candidates (rest nfinders))))))))

(defn -main
  [& args]
  (let [grid "700600008800030000090000310006740005005806900400092100087000020000060009600008001"
        xgrid (str-to-grid grid)
        pos1 (make-pos 1 1)
        pos1b (make-pos 1 1)
        pos2 (make-pos 1 3)
        pos3 (make-pos 3 1)
        pos4 (make-pos 5 5)]

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
    (def sudoku (Sudoku. grid))
    ;; (doseq [c (:candidates (.state sudoku))]
    ;;  (println "  " c))
    (.solve ^Sudoku sudoku)
    (print-grid (:solved @(.state sudoku)))
    (println "candidates left" (count (:candidates @(.state sudoku))))))
