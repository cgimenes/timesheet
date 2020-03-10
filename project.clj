(defproject timesheet "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clojure.java-time "0.3.2"]
                 [com.novemberain/monger "3.5.0"]
                ;  [org.clojure/java.jdbc "0.7.11"]
                ;  [org.postgresql/postgresql "42.2.10.jre7"]
                ;;  [org.clojure/data.json "1.0.0"]
                 [ring/ring-core "1.8.0"]
                 [ring/ring-jetty-adapter "1.8.0"]
                 [compojure "1.6.1"]
                 [selmer "1.12.18"]
                ;;  [org.liquibase/liquibase-core "3.8.7"
                  ;; :exclusions [ch.qos.logback/logback-classic]]
                ;;  [org.yaml/snakeyaml "1.26"]
                 ]
  :main ^:skip-aot timesheet.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
