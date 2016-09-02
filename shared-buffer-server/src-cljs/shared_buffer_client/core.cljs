(ns cljs.shared-buffer-client.core
  (:require [clojure.string :refer [join last-index-of replace-first split]]))

(def *editor* (.edit js/ace "editor"))
(.setReadOnly *editor* true)
(set! (.-$blockScrolling *editor*) js/Infinity)
(def *document* (-> *editor* (.getSession) (.getDocument)))
(def state (atom {:key js/sbKey :token 0 :seqno 0 :socket nil}))

(defn delta->op [delta]
  (let [pos    (-> *document* (.positionToIndex (aget delta "start")))
        type   (case (aget delta "action") "insert" :ins "remove" :del)
        string (join "\n" (aget delta "lines")) ]
    {:pos pos type string}))

(defn connect [s]
  (let [ws-addr (replace-first s "http" "ws")
        ws-addr (subs ws-addr 0 (last-index-of ws-addr "/"))]
    (js/WebSocket. ws-addr)))

(defn send! [msg]
  (->> msg clj->js (.stringify js/JSON) (.send (:socket @state))))

(defn send-op
  [delta]
  (send! {:type :operation
          :session (:key @state)
          :operation (delta->op delta)
          :token (:token @state)
          :seqno (:seqno @state)})
  (swap! state update :seqno inc))

(defn next-pos [p op]
  (let [pos (:pos op) ins (:ins op) del (:del op)]
    (cond (and (<= pos p) ins) (+ p (count ins))
          (and (< pos p) del) (max pos (- p (count del)))
          :else p)))

(defmulti apply-op (fn [s op] (if (:ins op) :ins :del)))

(defmethod apply-op :ins [s {pos :pos text :ins}]
  (str (subs s 0 pos) text (subs s pos)))

(defmethod apply-op :del [s {pos :pos text :del}]
  (str (subs s 0 pos) (subs s (+ pos (count text)))))

(defn apply-ops [ops]
  (.off *document* "change" send-op)
  (let [p (->> *editor* (.getCursorPosition)
               (.positionToIndex *document*))
        new-pos (reduce next-pos p ops)]
    (.setValue *document* (reduce apply-op (.getValue *document*) ops))
    (.moveCursorToPosition *editor* (.indexToPosition *document* new-pos)))
  (.on *document* "change" send-op))

(defmulti receive (comp keyword :type))

(defmethod receive :connect-response [msg]
  (swap! state assoc :key (:session msg)))

(defmethod receive :buffer-request [msg]
  (send! {:type :buffer-response
          :session (:key @state)
          :operation {:pos 0 :ins (.getValue *document*)}
          :token (:token @state)}))

(defmethod receive :operations [msg]
  (when (= (:seqno msg) (:seqno @state))
    (apply-ops (:operations msg))
    (swap! state assoc :token (:token msg)))
  (swap! state update :seqno inc))

(defmethod receive :default [msg]
  (.log js/console "default" (clj->js msg)))

(swap! state assoc :socket (connect js/document.location.href))

(set! (.-onopen (:socket @state))
      (fn [_]
        (send! {:type :connect-request
                :session (:key @state)})))

(set! (.-onmessage (:socket @state))
      (fn [data]
        (.setReadOnly *editor* false)
        (-> (.parse js/JSON (.-data data))
            (js->clj :keywordize-keys true)
            (receive))))

(.on *document* "change" send-op)
