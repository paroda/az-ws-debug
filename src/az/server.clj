(ns az.server
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]
            [gniazdo.core :as ws]
            [az.util :as u])
  (:import [java.net InetAddress ServerSocket Socket]
           [java.nio ByteBuffer]))

(def ^:private MAX_BUFF_SIZE 50000) ;; < keep small than websocket packet limit

(defonce ^:private servers (atom {}))

(defmacro ^:private thread
  [^String name daemon & body]
  `(doto (Thread. (fn [] ~@body) ~name)
     (.setDaemon ~daemon)
     (.start)))

;; returns websocket
(defn- az-connect [az-info & {:keys [on-close out-fn]}]
  (let [{:keys [az-app-name user-name password]} az-info
        a-out-fn (atom nil)
        ws (-> (str "wss://" (str/lower-case az-app-name)
                    ".scm.azurewebsites.net"
                    "/DebugSiteExtension/JavaDebugSiteExtension.ashx")
               (ws/connect :headers {"Authorization" (u/encode-auth user-name password)}
                           ;; :on-connect on-connect
                           ;; :on-receive on-receive
                           :on-binary #(@a-out-fn %1 %2 %3)
                           :on-error #(log/debug "sesson error" az-app-name %)
                           :on-close (or on-close (constantly nil))))]
    (reset! a-out-fn
            (fn [buf off len]
              (if (str/index-of (String. buf off len) ":repl/jdwp-handshake-over")
                (reset! a-out-fn out-fn))))
    (ws/send-msg ws "JDWP-Handshake") ;; first packet to satisfy JDWP
    (ws/send-msg ws "\n:repl/jdwp-handshake-over\n")
    ws))

(defn- accept-connection [^Socket conn name client-id]
  (if-let [{:keys [az-info]} (get @servers name)]
    (let [in (.getInputStream conn)
          out (.getOutputStream conn)
          ws (az-connect az-info
                         :on-close (fn [_ _] (.close conn))
                         :out-fn #(.write out %1 %2 %3))
          buf (byte-array MAX_BUFF_SIZE)]
      (swap! servers update-in [name :sessions] assoc client-id ws)
      (log/debug "socket repl connected" (:az-app-name az-info) client-id conn)
      ;; pass client data to webscoket
      (try
        (loop []
          (let [n (.read in buf 0 MAX_BUFF_SIZE)]
            (if (pos? n)
              (ws/send-msg ws (ByteBuffer/wrap buf 0 n)))
            (if (not= -1 n)
              (recur))))
        (catch Exception _disconnect))
      ;; finally cleanup, close websocket & client
      (swap! servers update-in [name :sessions] dissoc client-id)
      (log/debug "socket repl disconnected" (:az-app-name az-info) client-id conn)
      (ws/close ws)
      (.close conn))
    ;; can't proceed, close the connection
    (.close conn)))

(defn start-server
  ([az-info port]
   (start-server az-info nil port))
  ([az-info host port]
   (let [{:keys [az-app-name]} az-info
         address (InetAddress/getByName host)]  ;; nil returns loopback
     (if-let [{:keys [host port]} (get @servers az-app-name)]
       (log/error "Already connected to" az-app-name
                  (format "and listening at [%s:%s]" host port))
       ;; create session for terminal repl
       (if-let [ws (az-connect az-info
                               :out-fn (fn [buf off len]
                                         (when (get-in @servers [az-app-name :active?])
                                           (print (String. buf off len))
                                           (flush))))]
         (try
           ;; start socket repl server
           (let [socket (ServerSocket. port 0 address)
                 host (.getHostAddress address)]
             ;; register new server
             (swap! servers assoc az-app-name {:az-info az-info, :socket socket
                                               :host host, :port port
                                               :sessions {:term ws}})
             ;; listen for repl requests
             (thread
               (str "Clojure Server " az-app-name) true
               ;; accepts connections till socket is open
               (loop [client-counter 1]
                 (when (not (.isClosed socket))
                   (try
                     (let [conn (.accept socket)
                           client-id (str client-counter)]
                       (thread
                         (str "Clojure Connection " az-app-name " " client-id) true
                         (accept-connection conn az-app-name client-id)))
                     (catch Exception _disconnect))
                   (recur (inc client-counter))))
               ;; clean up in the end
               (ws/close ws)
               (swap! servers dissoc az-app-name)
               (log/info "Stopped Socket REPL server:" az-app-name))
             ;; socket REPL server ready
             (log/info "\nStarted Socket REPL server listening on " (format "[%s:%s]" host port)
                       "\n Azure App Name:" az-app-name
                       "\n For quick access call " (format "(az.ws-debug/repl \"%s\")" az-app-name)
                       "\n Or you can connect from any Socket REPL based client."))
           ;; close terminal repl session if unable to start server
           (catch Exception _
             (ws/close ws))))))))

(defn repl [az-app-name]
  (if (get @servers az-app-name)
    (try
      (print "\n=> ")
      (flush)
      (swap! servers update az-app-name assoc :active? true)
      (loop []
        (let [inp (read-line)
              ws (get-in @servers [az-app-name :sessions :term])]
          (cond
            (not ws) (log/error az-app-name "not connected!")
            (= inp ":repl/quit") (log/info az-app-name "repl closed!")
            :else (do
                    (ws/send-msg ws (str inp "\n"))
                    (recur)))))
      (finally
        (swap! servers update az-app-name dissoc :active?)))
    (log/error az-app-name "no connected!")))

(defn stop-server [az-app-name]
  (when-let [{:keys [socket sessions]} (get @servers az-app-name)]
    (doseq [ws (vals sessions)]
      (ws/close ws))
    (.close socket)))

(defn stop-all-servers []
  (doseq [az-app-name (keys @servers)]
    (stop-server az-app-name)))