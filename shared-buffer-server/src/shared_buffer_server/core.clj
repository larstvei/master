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
(declare include-clients)
(declare add-to-room)
(declare generate-key)
(declare handler)
(declare app)
(declare stop-server)
(declare -main)

;;; Variables

(defrecord Room [key clients uninitialized-clients min-seqno expected-seqno operations])

(def key-length
  "This dictates the length of random generated keys."
  8)

(def chan->key-map
  "A map where clients are mapped to the keys."
  (atom {}))

(def key->room-map
  "A map of all rooms, where a room is a set of clients. Each room is
  associated with a random-generated key."
  (atom {}))

(defonce server (atom nil))

;;; Socket communication

(defn initialize-client
  "The function is called on initialization. It adds the client to the
  chan->key-map."
  [client]
  (swap! chan->key-map assoc client nil))

(defn dissolve-client
  "The function is called when a connection to a client is closed. It
  removes the client. If the room its in has no more clients, the room is
  closed."
  [client status]
  (let [key (@chan->key-map client)
        room (@key->room-map key)
        clients (disj (:clients room) client)]
    (when key
      (if (empty? clients)
        (swap! key->room-map dissoc key)
        (swap! key->room-map assoc key
               (update-in room [:clients] disj client)))
      (swap! chan->key-map dissoc client))))

;;; Send

(defn distribute-change
  "Distribute a change to the room the client is in. The change is not sent
  to the client that made the change."
  [client msg]
  (let [room (@key->room-map (msg :key))
        clients (seq (disj (:clients room) client))
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
      "room" (add-to-room client (msg :room))
      "entire-buffer" (include-clients msg)
      "change" (distribute-change client msg)
      'error)))

(defn include-clients
  "Send the entire buffer to all uninitialized clients, add the
  uninitialized clients to clients and empty the uninitialized clients set."
  [msg]
  (let [key (msg :key)
        room (@key->room-map key)
        uninit-cli (:uninitialized-clients room)]
    (doseq [c (seq uninit-cli)] (send-addition c msg))
    (swap! key->room-map assoc key
           (-> room
               (update-in [:clients] union uninit-cli)
               (update-in [:uninitialized-clients] empty)))))

(defn add-to-room
  "Adds a connecting client to a room."
  [client key]
  (let [key (or key (generate-key 8))
        room (or (@key->room-map key)
                 (Room. key #{} #{} 0 1 []))]
    (when (empty? (:clients room))
      (send! client (json/write-str {:type 'room :room key})))
    (swap! chan->key-map assoc client key)
    (swap! key->room-map assoc key
           (update-in room [(if (empty? (:clients room))
                              :clients
                              :uninitialized-clients)] conj client))
    (when-not (empty? (:clients room))
      (send! (first (:clients room))
             (json/write-str {:type 'entire-buffer})))))

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
    (reset! chan->key-map {})
    (reset! key->room-map {})))

(defn -main
  "The main function for Shared Buffer. It simply starts the server."
  [& [args]]                            ; <- is this right?
  (reset! server (run-server #'handler {:port 3705})))
