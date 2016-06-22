(ns shared-buffer-server.syncprn)

(def lock (Object.))

(defn syncprn [& args]
  (locking lock (apply prn args)))
