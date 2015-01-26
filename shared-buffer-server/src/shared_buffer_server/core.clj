(ns shared-buffer-server.core
  (:gen-class)
  (:use org.httpkit.server)
  (:require [clojure.set :refer :all])
  (:require [clojure.data.json :as json])
  (:import java.security.SecureRandom
           [org.apache.commons.codec.binary Base64]))

;;; Declare functions

(declare initialize-client)
(declare dissolve-client)
(declare distribute-change)
(declare send-addition)
(declare send-deletion)
(declare receive)
(declare provide-room)
(declare generate-key)
(declare handler)
(declare app)
(declare stop-server)
(declare -main)

;;; Variables

(def key-length
  "This dictates the length of random generated keys."
  8)

(def key-map
  "A map where clients are mapped to the keys."
  (atom {}))

(def room-map
  "A map of all rooms, where a room is a set of clients. Each room is
  associated with a random-generated key."
  (atom {}))

(def server
  "A reference to the server, which is a function that stops the server."
  (atom nil))

;;; Socket communication

(defn initialize-client
  "The function is called on initialization. It adds the client to the
  key-map."
  [client]
  (swap! key-map assoc client nil))

(defn dissolve-client
  "The function is called when a connection to a client is closed. It
  removes the client. If the room its in has no more clients, the room is
  closed."
  [client status]
  (let [key (@key-map client)
        room (@room-map key)
        clients (disj room client)]
    (when key
      (if-not (empty? clients)
        (swap! room-map assoc key clients)
        (swap! room-map dissoc key))
      (swap! key-map dissoc client))))

;;; Send

(defn distribute-change
  "Distribute a change to the room the client is in. The change is not sent
  to the client that made the change."
  [client msg]
  (let [room (@room-map (msg :key))
        clients (seq (disj room client))
        send-f (cond (msg :addition) send-addition
                     (msg :bytes-deleted) send-deletion)]
    (when send-f (doseq [c clients] (send-f c msg)))))

(defn send-addition
  "Send an addition to a given client."
  [client msg]
  (let [msg (json/write-str msg)]
    (send! client msg)))

(defn send-deletion
  "Send a deletion to a given client."
  [client msg]
  (let [msg (json/write-str msg)]
    (send! client msg)))

;;; Receive

(defn receive
  "This function is called when msg is received from a connected
  client. data is a json-string that contains a message. It passes msg to a
  function depending on the type of the message."
  [client data]
  (let [msg (json/read-str data :key-fn keyword)]
    (case (msg :type)
      "room" (provide-room client (msg :room))
      "change" (distribute-change client msg)
      'error)))

(defn provide-room
  "Adds a connecting client to a room."
  [client room]
  (let [key (or room (generate-key 8))
        members (or (@room-map key) #{})]
    (when-not room
      (send! client (json/write-str {:type 'room :room key})))
    (swap! key-map assoc client key)
    (swap! room-map assoc key (conj members client))))

;;; Miscellaneous

(defn generate-key
  "Returns a pseudo-random url-friendly string with length given by
  `key-length'."
  [len]
  (Base64/encodeBase64URLSafeString
   (let [seed (byte-array len)]
     (.nextBytes (SecureRandom.) seed)
     seed)))

;;; Server

(defn handler
  "The handler for incoming requests."
  [req]
  (with-channel req channel
    (if (websocket? channel)
      (initialize-client channel)
      (send! channel (app req)))
    (on-close channel #(dissolve-client channel %))
    (on-receive channel #(receive channel %))))

(defn app
  "If the incoming request is not a socket, present an overview of the
  server."
  [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "This should provide a overview of the activity on the
  server."})

(defn stop-server
  "Stops the server, and resets all variables to their initial value."
  []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)
    (reset! key-map {})
    (reset! room-map {})))

(defn -main
  "The main function for Shared Buffer. It simply starts the server."
  [& [args]]                            ; <- is this right?
  (reset! server (run-server #'handler {:port 8080})))
