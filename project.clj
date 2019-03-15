(defproject cl-sudoku "0.1.0-SNAPSHOT"
  :description "sudoku solver"
  :license {:name "GNU Affero General Public License"
            :url "https://www.gnu.org/licenses/agpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot cl-sudoku.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
