(ns az.ws-debug
  (:require [clojure.string :as str]
            [gniazdo.core :as ws]
            [taoensso.timbre :as log])
  (:import [java.util Base64]))

(defonce state (atom {}))

(defn encode-auth [user-name password]
  (let [creds (format "%s:%s" user-name password)]
    (format "Basic %s"
            (-> (Base64/getEncoder)
                (.encodeToString (.getBytes creds))))))

(defn- on-connect [session]
  (swap! state assoc :session session)
  (log/info "session ready"))

(defn- printout [txt]
  (if (:jdwp-handshake-over? @state)
    (do
      (print txt)
      (flush))
    (if (str/index-of txt ":repl/jdwp-handshake-over")
      (swap! state assoc :jdwp-handshake-over? true)
      #_(log/debug "pre jdwp-handshake:" (pr-str txt)))))

(defn- on-receive [msg]
  (swap! state assoc :received msg)
  (printout msg))

(defn- on-binary [bytes offset length]
  (swap! state assoc :binary [bytes offset length])
  (printout (String. bytes offset length)))

(defn- on-error [error]
  (swap! state assoc :error error)
  (log/error error "Socket error!"))

(defn- on-close [status reason]
  (swap! state dissoc :session :socket)
  (log/info "session closed:" status reason))

(defn repl []
  (log/info "repl started")
  (print "=> ")
  (flush)
  (loop []
    (let [inp (read-line)
          socket (:socket @state)]
      (cond
        (not socket) (log/warn "not connected!")
        (= inp ":repl/quit") (log/info "repl closed!")
        :else (do
                (ws/send-msg socket (str inp "\n"))
                (recur))))))

(defn connect
  ([{:keys [az-app-name user-name password]}]
   (connect az-app-name user-name password)) 
  ([az-app-name user-name password]
   (some-> @state :socket ws/close) ;; close previous
   (reset! state {}) ;; clean up
   ;; connect
   (let [socket (-> (str "wss://" az-app-name
                         ".scm.azurewebsites.net"
                         "/DebugSiteExtension/JavaDebugSiteExtension.ashx")
                    (ws/connect :headers {"Authorization" (encode-auth user-name password)}
                                :on-connect on-connect
                                :on-receive on-receive
                                :on-binary on-binary
                                :on-error on-error
                                :on-close on-close))]
     (swap! state assoc :socket socket)
     (ws/send-msg socket "JDWP-Handshake") ;; first packet to satisfy JDWP
     (ws/send-msg socket "\n:repl/jdwp-handshake-over\n"))
   (log/info "session connected")))

(defn send-msg [msg]
  (if-let [s (:socket @state)]
    (ws/send-msg s msg)
    (log/warn "not connected!")))

(defn close []
  (if-let [s (:socket @state)]
    (ws/close s)
    (log/warn "not connected!")))
