(ns shared-buffer-server.utils
  (:import java.security.SecureRandom
           [org.apache.commons.codec.binary Base64]))

(defn dissoc-in
  "Dissociates a value in a nested associative structure, where ks is a
  sequence of keys and returns a new nested structure."
  [m ks]
  (update-in m (vec (butlast ks)) dissoc (last ks)))

(defn generate-key
  "Returns a pseudo-random url-friendly string with length given by
  `key-length'."
  [len]
  (Base64/encodeBase64URLSafeString
   (let [seed (byte-array len)]
     (.nextBytes (SecureRandom.) seed)
     seed)))
