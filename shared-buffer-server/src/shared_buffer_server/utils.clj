(ns shared-buffer-server.utils
  (:import java.security.SecureRandom
           [org.apache.commons.codec.binary Base64]))

(def key-length
  "This dictates the length of random generated keys."
  32)

(defn dissoc-in
  "Dissociates a value in a nested associative structure, where ks is a
  sequence of keys and returns a new nested structure."
  [m ks]
  (update-in m (vec (butlast ks)) dissoc (last ks)))

(defn generate-key
  "Returns a pseudo-random url-friendly string with length given by
  `key-length'."
  []
  (Base64/encodeBase64URLSafeString
   (let [seed (byte-array key-length)]
     (.nextBytes (SecureRandom.) seed)
     seed)))
