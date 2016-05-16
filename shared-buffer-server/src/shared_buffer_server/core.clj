(ns shared-buffer-server.core
  (:gen-class)
  (:use org.httpkit.server)
  (:require [shared-buffer-server.app :refer :all])
  (:require [shared-buffer-server.utils :refer :all])
  (:require [clojure.set :refer :all])
  (:require [clojure.data.json :as json]))

(defonce state (atom {}))

(def key-length
  "This dictates the length of random generated keys."
  8)

;;; Initialization and termination

(defn initialize-client
  "The function is called on initialization. It adds the client to the state."
  [state client]
  (assoc-in state [:clients client :id] (hash client)))

(defn join-room
  "The function is called when a client requests to join a room. It is added to
  the room unconditionally in the state."
  [state client room]
  (-> state
      (assoc-in  [:clients client :room] room)
      (update-in [:rooms room :clients] (fnil conj #{}) client)
      (update-in [:rooms room :state] (fnil identity 0))))

(defn dissolve-client
  "The function is called when a connection to a client is closed. It
  removes the client. If the room its in has no more clients, the room is
  closed."
  [state client status]
  (let [room (get-in state [:clients client :room])
        state (dissoc-in state [:clients client])]
    (if (= 1 (count (get-in state [:rooms room :clients])))
      (dissoc-in state [:rooms room])
      (update-in state [:rooms room :clients] disj client))))

;;; Receive

(defmulti receive (comp keyword :type))

(defmethod receive :room [msg client]
  (let [key (or (:room msg) (generate-key key-length))]
    (swap! state join-room client key)
    (when-not (:room msg)
      (->> {:type :room :room key}
           (json/write-str)
           (send! client)))))

(defmethod receive :operation [msg client]
  (let [o (select-keys msg [:pos :ins :del])
        i (:state msg)
        m (get-in @state [:rooms (:key msg) :state])
        u (get-in @state [:clients client :id])
        event [o i m u]]
    (println "event:" event)))

(defmethod receive :default [msg client]
  (println "default" msg))

;;; Handle requests

(defn reader [client]
  (fn [data]
    (json/read-str data :key-fn keyword)))

(defn handler
  "The handler for incoming requests."
  [req]
  (with-channel req channel
    (if (websocket? channel)
      (swap! state initialize-client channel)
      (send! channel (app req)))
    (on-close channel (partial swap! state dissolve-client channel))
    (on-receive channel (comp #(receive % channel) (reader channel)))))

;;; Server management

(defn stop-server
  "Stops the server, and resets all variables to their initial value."
  []
  (when-not (nil? (:server @state))
    ((:server @state) :timeout 100)
    (reset! state {})))

(defn -main
  "The main function for Shared Buffer. It simply starts the server."
  [& args]                            ; <- is this right?
  (swap! state assoc-in [:server] (run-server #'handler {:port 3705})))
