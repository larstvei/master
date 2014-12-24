(ns shared-buffer-server.core
  (:gen-class)
  (:use org.httpkit.server)
  (:require [clojure.data.json :as json])
  (:import java.security.SecureRandom
           [org.apache.commons.codec.binary Base64]))

(defn app [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "This should provide a overview of the activity on the server."})

(def clients
  "A set of clients."
  (atom #{}))

(def key-length
  "This dictates the length of random generated keys."
  8)

(defn generate-key [len]
  (Base64/encodeBase64URLSafeString
   (let [seed (byte-array len)]
     (.nextBytes (SecureRandom.) seed)
     seed)))

(defn provide-room [client room]
  (if room
    (println room)
    (send! client (json/write-str {:type 'room :room (generate-key 8)}))))

(defn receive [client data]
  (case (data "type")
    "room" (provide-room client (data 'room))
    "unknown"))

(defn dissolve-client [client status]
  (swap! clients disj client))

(defn initialize-client [client]
  (swap! clients conj client))

(defn handler [req]
  (with-channel req channel
    (if (websocket? channel)
      (initialize-client channel)
      (send! channel (app req)))
    ;; Log closing channel.
    (on-close channel #(dissolve-client channel %))
    ;; Echo on receive
    (on-receive channel #(receive channel (json/read-str %)))))

(defonce server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)
    (reset! clients nil)))

(defn -main [&args]
  ;; The #' is useful when you want to hot-reload code
  ;; You may want to take a look: https://github.com/clojure/tools.namespace
  ;; and http://http-kit.org/migration.html#reload
  (reset! server (run-server #'handler {:port 8080})))
