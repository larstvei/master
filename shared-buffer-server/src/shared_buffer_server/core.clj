(ns shared-buffer-server.core
  (:gen-class)
  (:use org.httpkit.server)
  (:require [shared-buffer-server.app :refer :all]
            [shared-buffer-server.utils :refer :all]
            [shared-buffer-server.history :refer :all]
            [clojure.data.json :as json]))

(defonce state (atom {:sessions {} :clients {}}))

(defn clients-in-session [state key]
  "Given a session key, return the number of clients the session."
  (-> state :sessions key :clients count))

(defn empty-session? [state key]
  "Given a key, return true if there are no clients in the session."
  (zero? (clients-in-session state key)))

(defn initialized? [state client]
  "Returns true if the given client is initialized."
  (get-in state [:clients client :initialized]))

(defn get-initialized-client [state key]
  "Get an initialized client from a session identified by key."
  (->> state :sessions key :clients
       (filter (partial initialized? state)) rand-nth))

(defn get-uninitialized-clients [state key]
  "Get all uninitialized clients from a session identified by key."
  (->> state :sessions key :clients
       (remove (partial initialized? state))))

(defn min-token [session]
  "Get the smallest state token among clients in the session."
  (->> session :tokens vals (apply min)))

(defn next-seq [n m]
  "Given the a stored sequence number for a client and the sequence number of a
  message, calculate the next sequence number for the client."
  (+ (inc n) (- m n)))

(defn initialize-client
  "Adds a given client to the state."
  [state client]
  (-> state
      (assoc-in [:clients client :id] (hash client))
      (assoc-in [:clients client :seqno] 0)))

(defn join-session
  "Add a client to the session unconditionally."
  [state client key]
  (-> state
      (assoc-in  [:clients client :session] key)
      (assoc-in  [:clients client :initialized] (empty-session? state key))
      (assoc-in  [:sessions key :tokens client] 0)
      (update-in [:sessions key :clients] (fnil conj #{}) client)
      (update-in [:sessions key :token] (fnil identity 1))
      (update-in [:sessions key :lock] (fnil identity (Object.)))))

(defn dissolve-client
  "Remove a given client from the state. If this is the only client in the
  session, then dissolve the session."
  [state client status]
  (let [key   (get-in state [:clients client :session])
        state (dissoc-in state [:clients client])]
    (if (= 1 (count (get-in state [:sessions key :clients])))
      (dissoc-in state [:sessions key])
      (update-in state [:sessions key :clients] disj client))))

(defn update-client [state client seqno op f]
  "Update the sequence number and list of (possibly) rejected operations of a
  given client. Function f is either conj or a function that replaces the
  list."
  (-> state
      (assoc-in  [:clients client :seqno] seqno)
      (update-in [:clients client :ops] f op)))

(defn update-session [state key history]
  "Update a session with a new history, and increment it's state token."
  (-> state
      (assoc-in  [:sessions key :history] history)
      (update-in [:sessions key :token] inc)))

(defn next-state [state client token seqno op history]
  "Updates the session and the client, and trims the history."
  (let [key (get-in state [:clients client :session])
        t   (-> state :sessions key min-token)]
    (-> state
        (update-client client seqno op (fn [_ x] (list x)))
        (assoc-in [:sessions key :tokens client] token)
        (update-session key (trim-history history t)))))

;;; Send

(defn make-msg [key op seqno token]
  "Given a operation, sequence number and token, generate a message."
  {:type :operations
   :session key
   :operations (reverse op)
   :seqno seqno
   :token token})

(defn send-op!
  "Sends an operation to a set of clients."
  [key op token clients]
  (doseq [c clients]
    (let [site (get-in @state [:clients c])
          seqno (:seqno site)
          msg (make-msg key op seqno token)]
      (when (get-in @state [:clients c :initialized])
        (send! c (json/write-str msg)))
      (swap! state update-client c (inc seqno) [op token] conj))))

(defn send-buffer-request [state key]
  "Send a buffer request to some initialized client."
  (->> {:type :buffer-request :session key}
       (json/write-str)
       (send! (get-initialized-client state key))))

;;; Receive

(defmulti receive
  "Receive is dispatched on the message type."
  (comp keyword :type))

(defmethod receive :buffer-response [msg client]
  "Given a buffer, send an operation to all uninitialized clients, making them
  consistent with the current history."
  (locking (get-in @state [:sessions (keyword (:session msg)) :lock])
    (let [key (keyword (:session msg))
          op  (list (-> msg :operation))
          ops (get-in @state [:clients client :ops])
          op2 (make-initial-op op ops (:token msg))
          msg (make-msg key op2 0 (-> @state :sessions key :token))]
      (doseq [c (get-uninitialized-clients @state key)]
        (send! c (json/write-str msg))
        (swap! state assoc-in  [:clients c :initialized] true)
        (swap! state update-in [:clients c :seqno] inc)))))

(defmethod receive :connect-request [msg client]
  "Given a connect-request, add the client to the session specified in message,
  or generate a new session. If the session is ongoing, then fetch the buffer
  from a client."
  (let [key (or (:session msg) (generate-key))]
    (swap! state join-session client (keyword key))
    (when (or (not (:session msg))
              (= 1 (clients-in-session @state (keyword key))))
      (->> {:type :connect-response :session key}
           (json/write-str)
           (send! client)))
    (when-not (initialized? @state client)
      (send-buffer-request @state (keyword key)))))

(defmethod receive :operation [msg client]
  "Given a new operation, add the operation to the history, and send operations
  to all clients, making them consistent with the current history."
  (locking (get-in @state [:sessions (keyword (:session msg)) :lock])
    (let [key     (keyword (:session msg))
          session (get-in @state [:sessions key])
          site    (get-in @state [:clients client])
          op      (list (-> msg :operation))
          seqno   (next-seq (:seqno msg) (:seqno site))
          token   (:token msg)
          time    (inc (:token session))
          event   [op token (:token session) (:id site)]
          hist    (add-event (:history session) event)
          op1     (make-op (:history session) hist token)
          op2     (make-response op op1 (:ops site) token)
          reply   (make-msg key op2 seqno time)
          rest    (disj (:clients session) client)]
      (send-op! key op1 time rest)
      (send! client (json/write-str reply))
      (swap! state next-state client token (inc seqno) [op2 time] hist))))

(defmethod receive :default [msg client]
  "If none of the above, then print the message for debugging purposes."
  (println "undefined message type" msg))

;;; Handle requests

(defn receiver [client]
  "Returns a function that, that reads the json string and calls receive."
  (fn [data]
    (-> data
        (json/read-str :key-fn keyword)
        (receive client))))

(defn handler
  "The handler for incoming requests."
  [req]
  (with-channel req channel
    (if (websocket? channel)
      (swap! state initialize-client channel)
      (send! channel (app req)))
    (on-close channel (partial swap! state dissolve-client channel))
    (on-receive channel (receiver channel))))

;;; Server management

(defn stop-server
  "Stops the server, and resets all variables to their initial value."
  []
  (when-not (nil? (:server @state))
    ((:server @state) :timeout 100)
    (reset! state {:sessions {} :clients {}})))

(defn -main
  "The main function for Shared Buffer. It simply starts the server."
  [& args]
  (swap! state assoc :server (run-server #'handler {:port 3705})))
