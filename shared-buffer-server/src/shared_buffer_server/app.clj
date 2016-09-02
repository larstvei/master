(ns shared-buffer-server.app
  (:require [compojure.route :as route]
            [compojure.core :refer [GET defroutes]]
            [ring.util.response :refer [redirect]]
            [clojure.java.io :as io]
            [hiccup.page :refer [html5 include-css include-js]]
            [shared-buffer-server.utils :refer [generate-key]]))

(defn new-editor [key]
  (html5
   [:head [:title "Shared Buffer"]
    (include-css "/css/editor.css")
    (include-js "/js/ace/ace.js")
    [:script {:type "text/javascript"}
     (format "var sbKey = \"%s\";" key)]]
   [:body [:pre {:id "editor"} ""]]
   (include-js "/js/main.js")))

(defroutes app
  (GET  "/:key" [key] (new-editor key))
  (GET  "/" []  (redirect (generate-key)))
  (route/resources "/")
  (route/not-found "Page not found"))
