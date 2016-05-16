(ns shared-buffer-server.app)

(defn app
  "If the incoming request is not a socket, present an overview of the
  server."
  [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "This should provide a overview of the activity on the server."})
