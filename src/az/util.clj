(ns az.util
  (:import [java.util Base64]))

(defn encode-auth [user-name password]
  (let [creds (format "%s:%s" user-name password)]
    (format "Basic %s"
            (-> (Base64/getEncoder)
                (.encodeToString (.getBytes creds))))))
