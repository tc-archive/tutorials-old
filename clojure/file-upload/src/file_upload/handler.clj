(ns file-upload.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]])
  (:require [clojure.java.io :as io]))

(defroutes app-routes
  
  (GET "/" [] "Hello World")

  (POST "/upload"
     {{{tempfile :tempfile filename :filename} :file} :params :as params}
     (io/copy tempfile (io/file "resources" "public" filename))
     "Success")

  (route/not-found "Not Found"))


#_(def app
  (wrap-defaults app-routes site-defaults))

(def app
  (wrap-defaults app-routes
    (assoc-in site-defaults [:security :anti-forgery] false)))
