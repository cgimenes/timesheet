(defproject timesheet "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clojure.java-time "0.3.2"]
                 [com.novemberain/monger "3.5.0"]
                 [ring/ring-core "1.8.0"]
                 [ring/ring-jetty-adapter "1.8.0"]
                 [ring-cors/ring-cors "0.1.13"]
                 [compojure "1.6.1"]
                 [philoskim/debux "0.6.8"]
                 [ring/ring-json "0.5.0"]
                 [clj-http "3.10.1"]
                 [buddy/buddy-auth "2.2.0"]
                 [cheshire "5.10.0"]]
  :main ^:skip-aot timesheet.main
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
