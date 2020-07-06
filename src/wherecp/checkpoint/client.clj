(ns wherecp.checkpoint.client
  (:gen-class)
  (:require [clj-http.client :as http-client]
            [cheshire.core :as json]
            [wherecp.models :as models]))

(defn get-sid [args]
  (json/parse-string
   (:body (http-client/post
           (str "https://" (args :ip) "/web_api/v1.5/login")
           {:content-type :json
            :body
            (json/generate-string
             {:user (args :user)
              :password (args :password)})
            :insecure? true}))
   true))

(defn create-host [sid sms-ip host]
  (http-client/post (str "https://" sms-ip "/web_api/v1.5/add-host")
                    {:content-type :json
                     :headers {:x-chkp-sid sid}
                     :body (json/generate-string
                            {:name (get host :name)
                             :ipv4-address (get host :ip)})
                     :insecure? true}))

(defn publish [sid sms-ip]
  (http-client/post (str "https://" sms-ip "/web_api/v1.5/publish")
                    {:content-type :json
                     :headers {:x-chkp-sid sid}
                     :body "{}"
                     :insecure? true}))

(defn get-objects
  [sid sms-ip url]
  (loop [offset 0
         total 0
         objects '()]
    (let [resp (json/parse-string
               (:body (http-client/post
                       (str "https://" sms-ip "/web_api/v1.5/" url)
                       {:content-type :json
                        :headers {:x-chkp-sid sid}
                        :body (json/generate-string {:limit 500
                                                     :offset offset})
             :insecure? true}))
               true)]
     (if (= (:total resp) (:to resp))
       (into objects (:objects resp))
       (recur (:to resp) (:total resp) (into objects (:objects resp)))))))

(defn get-hosts
  [sid sms-ip]
  (get-objects sid sms-ip "show-hosts"))

(defn get-networks
  [sid sms-ip]
  (get-objects sid sms-ip "show-networks"))
    
(defn get-ranges
  [sid sms-ip]
  (get-objects sid sms-ip "show-address-ranges"))

(defn parse-hosts [hosts]
  (map #(models/make-host (% :name) (models/convert-ip (% :ipv4-address)) (% :uid)) hosts))

(defn parse-networks [networks]
  (map #(if (contains? % :subnet4)
          (models/make-network
           (% :name)
           (models/convert-ip (% :subnet4))
           (models/convert-ip (% :subnet-mask))
           (% :uid))) networks))

(defn parse-ranges [ranges]
  (map #(if (contains? % :ipv4-address-first)
          (models/make-range (% :name)
                             (models/convert-ip (% :ipv4-address-first))
                             (models/convert-ip (% :ipv4-address-last))
                             (% :uid))) ranges))
