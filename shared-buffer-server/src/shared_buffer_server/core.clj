(ns shared-buffer-server.core
  (:gen-class)
  (:use org.httpkit.server)
  (:require [shared-buffer-server.app :refer :all])
  (:require [shared-buffer-server.utils :refer :all])
  (:require [shared-buffer-server.history :refer :all])
  (:require [shared-buffer-server.syncprn :refer [syncprn]])
  (:require [clojure.data.json :as json]))

(defonce state (atom {}))

(def key-length
  "This dictates the length of random generated keys."
  8)

;;; Initialization and termination

(defn initialize-client
  "The function is called on initialization. It adds the client to the state."
  [state client]
  (-> state
      (assoc-in [:clients client :id] (hash client))
      (assoc-in [:clients client :seqno] 0)
      (assoc-in [:clients client :token] 0)))

(defn join-room
  "The function is called when a client requests to join a room. It is added to
  the room unconditionally in the state."
  [state client key]
  (-> state
      (assoc-in  [:clients client :room] key)
      (update-in [:rooms key :clients] (fnil conj #{}) client)
      (update-in [:rooms key :token] (fnil identity 0))
      (update-in [:rooms key :lock] (fnil identity (Object.)))))

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

(defn update-client [state client seqno op f]
  (-> state
      (assoc-in  [:clients client :seqno] seqno)
      (update-in [:clients client :ops] f op)))

(defn update-room [state room history]
  (-> state
      (assoc-in  [:rooms room :history] history)
      (update-in [:rooms room :token] inc)))

(defn next-state [state client seqno op history]
  (let [room (get-in state [:clients client :room] )]
    (-> state
        (update-client client seqno op (fn [_ x] (list x)))
        (update-room room history (map :(:clients room))))))

;;; Send

(defn make-msg [op seqno token]
  {:type :operations
   :operations (reverse op)
   :seqno seqno
   :token token})

(defn send-op
  "Sends an operation to a set of clients."
  [op token clients]
  (doseq [c clients]
    (let [clinfo (get-in @state [:clients c])
          seqno (:seqno clinfo)
          msg (make-msg op seqno token)]
      (send! c (json/write-str msg))
      (swap! state update-client c (inc seqno) [op token] conj))))

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
  (locking (get-in @state [:rooms (:room msg) :lock])
    (let [room (get-in @state [:rooms (:room msg)])
          cli  (get-in @state [:clients client])
          op (list (select-keys msg [:pos :ins :del]))
          seqno (+ (inc (:seqno msg)) (- (:seqno cli) (:seqno msg)))
          time (inc (:token room))
          event [op (:token msg) (:token room) #{(:id cli)}]
          history (add-event (:history room) event)
          op1 (make-op (:history room) history (:token msg))
          op2 (make-response op op1 (:ops cli) (:token msg))
          reply (make-msg op2 seqno time)
          others (disj (:clients room) client)]
      (syncprn "broadcast" op1)
      (syncprn "reply" op2)
      (send-op op1 time others)
      (send! client (json/write-str reply))
      (swap! state next-state client (inc seqno) [op2 time] history))))

(defmethod receive :default [msg client]
  (println "default" msg))

;;; Handle requests

(defn reader [client]
  (fn [data]
    (let [res (json/read-str data :key-fn keyword)]
      (syncprn res)
      res)))

(defn handler
  "The handler for incoming requests."
  [req]
  (with-channel req channel
    ;; TODO: make synchronous initialization.
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
  (swap! state assoc :server (run-server #'handler {:port 3705})))
