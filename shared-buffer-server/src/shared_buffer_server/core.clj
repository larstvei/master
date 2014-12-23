(ns shared-buffer-server.core
  (:gen-class)
  (:use org.httpkit.server)
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

(defn receive [client data]
  (send! client data))

(defn dissolve-client [client status]
  (swap! clients disj client))

(defn handler [req]
  (with-channel req channel
    (if (websocket? channel)
      (swap! clients conj channel)
      (send! channel (app req)))
    ;; Log closing channel.
    (on-close channel #(dissolve-client channel %))
    ;; Echo on receive
    (on-receive channel #(receive channel %))))

(defonce server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn -main [&args]
  ;; The #' is useful when you want to hot-reload code
  ;; You may want to take a look: https://github.com/clojure/tools.namespace
  ;; and http://http-kit.org/migration.html#reload
  (reset! server (run-server #'handler {:port 8080})))
