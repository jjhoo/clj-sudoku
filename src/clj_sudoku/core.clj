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

(deftype Box [^byte row ^byte column]
  Object (toString [_]
           (str "(" row " " column ")")))

(deftype Pos [^byte row ^byte column ^Box box]
  Object (toString [_]
           (str "(" row " " column " " box ")")))

(deftype Cell [^byte value ^Pos pos]
  Object (toString [_]
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
  (map-indexed (fn [i c]
                 (let [v (- (byte c) (byte \0))
                       row (+ (/ i 9) 1)
                       col (+ (rem i 9) 1)]
                   (make-cell v row col)))
               (seq grid)))

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

(defn -main
  [& args]
  (let [grid "700600008800030000090000310006740005005806900400092100087000020000060009600008001"
        xgrid (str-to-grid grid)]

    ;; (doseq [x (init-candidates xgrid)]
    ;; (println "candidate" x))

    ;; (doseq [item xgrid]
    ;;  (println item))

    (print-grid xgrid)
    (println (grid-to-string xgrid))))
