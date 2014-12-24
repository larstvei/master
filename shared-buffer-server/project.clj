(defproject shared-buffer-server "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "GNU General Public License"
            :url "http://www.gnu.org/licenses/gpl-3.0.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.json "0.2.5"]
                 [http-kit "2.1.16"]
                 [commons-codec "1.6"]]
  :main ^:skip-aot shared-buffer-server.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
