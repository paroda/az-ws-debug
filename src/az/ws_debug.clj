(ns az.ws-debug
  "Utility for Live-debugging Clojure web applications hosted on Azure App Services."
  (:require [clojure.string :as str]
            [clojure.xml :as xml]
            [taoensso.timbre :as log]
            [az.basic :as bs]
            [az.server :as srv]))

(log/set-level! :info)

(defn parse-publish-settings [file]
  (->> (xml/parse file)
       :content
       (some #(if (and (= (:tag %) :publishProfile)
                       (= (get-in % [:attrs :publishMethod]) "MSDeploy"))
                (let [{:keys [attrs]} %]
                  {:az-app-name (:msdeploySite attrs)
                   :user-name (:userName attrs)
                   :password (:userPWD attrs)})))))

(defn- get-az-info [azure-settings]
  (let [az-info (if (map? azure-settings)
                  azure-settings
                  (parse-publish-settings azure-settings))]
    (if (or (str/blank? (:az-app-name az-info))
            (str/blank? (:user-name az-info))
            (str/blank? (:password az-info)))
      (log/error "Missing on or more of required info: az-app-name, user-name, password")
      az-info)))

(defn connect
  ([azure-settings]
   (if-let [az-info (get-az-info azure-settings)]
     (bs/connect az-info)))
  ([azure-settings port]
   (connect azure-settings nil port))
  ([azure-settings host port]
   (if-let [az-info (get-az-info azure-settings)]
     (srv/start-server az-info host port))))

(defn repl
  ([] (bs/repl))
  ([az-app-name] (srv/repl az-app-name)))

(defn close
  ([] (bs/close))
  ([az-app-name] (srv/stop-server az-app-name)))

(defn close-all []
  (bs/close)
  (srv/stop-all-servers))