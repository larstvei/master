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

(def client-to-key-map
  "A map where clients are mapped to the keys."
  (atom {}))

(def key-to-room-map
  "A map of all rooms, where a room is a set of clients. Each room is
  associated with a random-generated key."
  (atom {}))

(def key-length
  "This dictates the length of random generated keys."
  8)

(defn generate-key [len]
  (Base64/encodeBase64URLSafeString
   (let [seed (byte-array len)]
     (.nextBytes (SecureRandom.) seed)
     seed)))

(defn provide-room [client room]
  (let [key (or room (generate-key 8))
        members (or ((deref key-to-room-map) key) #{})]
    (when-not room
      (send! client (json/write-str {:type 'room :room key})))
    (swap! client-to-key-map assoc client key)
    (swap! key-to-room-map assoc key (conj members client))))

(defn receive [client data]
  (let [data (json/read-str data :key-fn keyword)]
    (case (data :type)
      "room" (provide-room client (data :room))
      "not-supported")))

(defn dissolve-client [client status]
  (let [key ((deref client-to-key-map) client)
        room ((deref key-to-room-map) key)]
    (when key
      (swap! key-to-room-map assoc key (disj room client))
      (swap! client-to-key-map assoc client nil))))

(defn initialize-client [client]
  (swap! client-to-key-map assoc client nil))

(defn handler [req]
  (with-channel req channel
    (if (websocket? channel)
      (initialize-client channel)
      (send! channel (app req)))
    ;; Log closing channel.
    (on-close channel #(dissolve-client channel %))
    ;; Echo on receive
    (on-receive channel #(receive channel %))))

(defonce server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)
    (reset! client-to-key-map {})
    (reset! key-to-room-map {})))

(defn -main [&args]
  ;; The #' is useful when you want to hot-reload code
  ;; You may want to take a look: https://github.com/clojure/tools.namespace
  ;; and http://http-kit.org/migration.html#reload
  (reset! server (run-server #'handler {:port 8080})))
