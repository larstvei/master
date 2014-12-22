(ns shared-buffer-server.core
  (:gen-class)
  (:use org.httpkit.server))

;; (defn handler [request]
;;   (with-channel request channel
;;     (on-close channel (fn [status] (println "channel closed: " status)))
;;     (on-receive channel (fn [data] ;; echo it back
;;                           (send! channel data)))))

(defn app [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "This should provide a overview of the activity on the server."})

(defn handler [req]
  (with-channel req channel              ; get the channel
    ;; communicate with client using method defined above
    (on-close channel (fn [status] (println "channel closed")))
    (if (websocket? channel)
      (println "WebSocket channel")
      (send! channel (app req)))
    (on-receive
     channel
     (fn [data]    ; data received from client
       ;; An optional param can pass to send!: close-after-send?
       ;; When unspecified, `close-after-send?` defaults to true for HTTP channels
       ;; and false for WebSocket.  (send! channel data close-after-send?)
       (send! channel data)))))        ; data is sent directly to the client

(defonce server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    ;; graceful shutdown: wait 100ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (@server :timeout 100)
    (reset! server nil)))

(defn -main [&args]
  ;; The #' is useful when you want to hot-reload code
  ;; You may want to take a look: https://github.com/clojure/tools.namespace
  ;; and http://http-kit.org/migration.html#reload
  (reset! server (run-server #'handler {:port 8080})))
