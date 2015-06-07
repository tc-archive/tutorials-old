(ns ring-tutorial.core
  (:require [ring.middleware.resource :refer :all]
            [ring.middleware.content-type :refer :all]
            [ring.middleware.params :refer :all]
            [ring.middleware.keyword-params :refer :all]
            [ring.middleware.multipart-params :refer :all]
            [ring.middleware.not-modified :refer :all]
            [ring.middleware.cookies :refer :all]
            [ring.middleware.session :refer :all]
            [ring.middleware.reload :refer :all]
            [ring.middleware.stacktrace :refer :all]
            )
  (:use ring.adapter.jetty))


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


;; Named Ring Handler
(defn hello-world-handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "Hello World"})


;; Named Ring Handler
(defn what-is-my-ip-handler [request]
  {:status 200
   :headers {"Content-Type" "text/plain"}
   :body (:remote-addr request)})


;; Named Ring Handler
(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/plain"}
   :body "I am a default ring handler wrapped on middleware!"})
;(defn handler [request]
;  ("wobble"))

;; Configure the default handler with middleware.
(def app
  (-> #'handler
      ;; https://github.com/ring-clojure/ring/wiki/Reloading
      (wrap-reload)
      ;(wrap-reload '(ring-tutorial.core))
      ;; https://mmcgrana.github.io/2010/03/clojure-web-development-ring.html
      (wrap-stacktrace)
      ;; https://github.com/ring-clojure/ring/wiki/Static-Resources
      (wrap-resource "public")
      ;; https://github.com/ring-clojure/ring/wiki/Content-Types
      (wrap-content-type)
      ;; https://github.com/ring-clojure/ring/wiki/Parameters
      ;(wrap-params)
      (wrap-keyword-params)
      ;; https://github.com/ring-clojure/ring/wiki/File-Uploads
      (wrap-multipart-params)
      ;; https://github.com/ring-clojure/ring/wiki/Static-Resources
      (wrap-not-modified)
      ;; https://github.com/ring-clojure/ring/wiki/Cookies
      (wrap-cookies)
      ;; https://github.com/ring-clojure/ring/wiki/Sessions
      (wrap-session)
      ))

;; Start Ring Handler 'hander' wth Jetty
;(defn boot-handdler []
;  (run-jetty #'handler {:port 8080}))

;; Start Ring Handler 'app' wth Jetty
(defn boot-app []
  (run-jetty #'app {:port 8080}))

