(defproject clj-sudoku "0.1.0-SNAPSHOT"
  :description "sudoku solver"
  :license {:name "GNU Affero General Public License"
            :url "https://www.gnu.org/licenses/agpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/math.combinatorics "0.1.6"]]
  :main clj-sudoku.core
  :aot [clj-sudoku.core]
  :target-path "target/%s"
  :plugins [[lein-cljfmt "0.6.4"]
            [jonase/eastwood "0.3.6"]]
  :global-vars {*warn-on-reflection* true}
  :profiles {:uberjar {:aot :all}})
