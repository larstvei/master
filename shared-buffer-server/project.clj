(defproject shared-buffer-server "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "GNU General Public License"
            :url "http://www.gnu.org/licenses/gpl-3.0.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha5"]
                 [org.clojure/data.json "0.2.6"]
                 [http-kit "2.1.18"]
                 [commons-codec "1.6"]
                 [compojure "1.5.1"]
                 [hiccup "1.0.5"]
                 [org.clojure/clojurescript "1.9.89"]]

  :plugins [[lein-cljsbuild "1.1.3"]]
  :cljsbuild {:builds [{
                        ;; The path to the top-level ClojureScript source directory:
                        :source-paths ["src-cljs"]
                        ;; The standard ClojureScript compiler options:
                        ;; (See the ClojureScript compiler documentation for details.)
                        :compiler {:output-to "resources/public/js/main.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}]}

  :main ^:skip-aot shared-buffer-server.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
