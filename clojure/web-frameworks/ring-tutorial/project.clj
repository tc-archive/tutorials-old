(defproject ring-tutorial "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.6.0"]
                 [ring/ring-core "1.3.2"]
                 [ring/ring-jetty-adapter "1.3.2"]
                 [ring/ring-devel "1.4.0-RC1"]

                 ;; The is the de facto Clojure logging library. It uses
                 ;; log4j under the hood, so we'll include that below.
                 ;; Leiningen can usually figure out these dependencies,
                 ;; but we're going to add an entry for log4j because we
                 ;; want to tell Leiningen to exclude the pieces of it
                 ;; we don't need.
                 [org.clojure/tools.logging "0.2.3"]
                 [log4j "1.2.16" :exclusions [javax.mail/mail
                                              javax.jms/jms
                                              com.sun.jdmk/jmxtools
                                              com.sun.jmx/jmxri]]
                 ]
  :dev-dependencies [
                 [ring/ring-devel "1.4.0-RC1"]
                 ]
  :plugins [[lein-ring "0.8.11"]]

  ;; configure lein-ring main handler
  :ring {:handler ring-tutorial.core/handler}

  :jvm-opts ["-server"
             "-Xms32M"
             "-Xmx256M"
             "-XX:NewRatio=5"
             "-XX:+UseConcMarkSweepGC"
             "-XX:+UseParNewGC"
             "-XX:MaxPermSize=64m"])
