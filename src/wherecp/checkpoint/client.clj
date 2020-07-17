(ns wherecp.checkpoint.client
  (:gen-class)
  (:require [clj-http.client :as http-client]
            [cheshire.core :as json]
            [wherecp.models :as models]
            [wherecp.store :as store]
            [clojure.string :as s])
  (:use
   [slingshot.slingshot :only [throw+ try+]]))

(defn get-sid [sms-ip creds]
  "Creates a session on the specified Checkpoint SMS and returns the session id (sid).
    sms-ip: IP address of the Checkpoint SMS.  
    creds: {:user :password}"
  (:sid
   (json/parse-string
    (:body
     (try+ (http-client/post
            (str "https://" sms-ip "/web_api/v1.5/login")
            {:content-type :json
             :body
             (json/generate-string
              {:user (creds :user)
               :password (creds :password)})
             :insecure? true})
           (catch [:status 400] {:keys [body]}
             (let [parsed-body (json/parse-string body true)]
               (throw+ {:type ::bad-auth
                        :message (:message parsed-body)
                        :code (:code parsed-body)})))))
    true)))

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
  [sid sms-ip url body]
  (loop [offset 0
         total 0
         objects '()]
    (let [resp (json/parse-string
               (:body (http-client/post
                       (str "https://" sms-ip "/web_api/v1.5/" url)
                       {:content-type :json
                        :headers {:x-chkp-sid sid}
                        :body (json/generate-string (merge {:limit 500
                                                     :offset offset} body))
             :insecure? true}))
               true)]
     (if (= (:total resp) (:to resp))
       (into objects (:objects resp))
       (recur (:to resp) (:total resp) (into objects (:objects resp)))))))

(defn get-hosts
  [sid sms-ip]
  (get-objects sid sms-ip "show-hosts" {}))

(defn get-networks
  [sid sms-ip]
  (get-objects sid sms-ip "show-networks" {}))

(defn get-ranges
  [sid sms-ip]
  (get-objects sid sms-ip "show-address-ranges" {}))

(defn get-groups
  [sid sms-ip]
  (get-objects sid sms-ip "show-groups" {:details-level "full"}))

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


(defn parse-network-groups [groups]
  (map
   #(models/make-group (:name %)
                       :network
                       '()
                       (:uid %))
   groups))

(defn populate-groups [groups store]
  (store/bulk-insert
   store
   (map
    (fn [group]
      (loop [current-group (first (store/get-by store #(= (:uid group) (:nid %))))
             members (:members group)]
        (if (empty? members)
          current-group
        (recur (models/add-group-member
                current-group
                (first (store/get-by store #(= (first members) (:nid %)))))
               (rest members)))))
    groups)))
                          
(defn get-tcp-services
  [sid sms-ip]
  (get-objects sid sms-ip "show-services-tcp" {}))
        
(defn parse-tcp-services
  [services]
  (map #(if (s/includes? (:port %) "-")
          (let [range (s/split (:port %) #"-")]
            models/make-service-range
            (:name %)
            models/tcp
            (models/str->int (nth range 0))
            (models/str->int (nth range 1))
            (:uid %))
          (models/make-service
           (:name %)
           models/tcp
           (models/str->int (:port %))
           (:uid %)))
          services))

(defn get-udp-services
  [sid sms-ip]
  (get-objects sid sms-ip "show-services-udp" {}))
        
(defn parse-udp-services
  [services]
  (map #(if (s/includes? (:port %) "-")
          (let [range (s/split (:port %) #"-")]
            models/make-service-range
            (:name %)
            models/udp
            (models/str->int (nth range 0))
            (models/str->int (nth range 1))
            (:uid %))
          (models/make-service
           (:name %)
           models/udp
           (models/str->int (:port %))
           (:uid %)))
          services))

(defn parse-service-groups [groups]
  (map
   #(models/make-group (:name %)
                       :service
                       '()
                       (:uid %))
   groups))

(defn get-rulebase
  [sid sms-ip rulebase-name]
  (get-objects sid sms-ip "show-access-rulebase" {:name rulebase-name}))
