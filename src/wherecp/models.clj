(ns wherecp.models
  (:gen-class)
  (:require [clojure.string :as str]
            [wherecp.store :as store]
            [clojure.string :as string]))

(defrecord Common-Network [^Long start ^Long end])
(defrecord Common-Service [proto start end])

(defprotocol Network-Object 
  (common-form [object] "Returns the object as a vector of Common network objects"))

(defprotocol Service-Object
  (common-svc-form [object] "Returns the object as a vector of Common Service objects"))

(defprotocol Matchable
  (match? [this that] "Returns true if both this object matches that."))

(defrecord Host [uid name type address nid]
  Network-Object
  (common-form [object] 
    ""
    (vector (->Common-Network (get object :address) (get object :address))))
  Matchable
  (match? [this that]
    (and (= (get this :type) (get that :type))
         (= (get this :address) (get that :address))))
  store/Serializable
  (serialize [object] 
    object)
  store/De-Serializable
  (de-serialize [object store]
    object))

(defn make-uuid [] (str (java.util.UUID/randomUUID)))

(def long-mask 4294967295)

(def ip-max 4294967295)

(defn is-host? [h]
  (= (get h :type) :host))

(defn make-host
  ([uid name address nid] 
   (->Host (keyword uid) 
           name
           :host
           (bit-and address long-mask)
           nid))
  ([name address nid] 
   (make-host (keyword (make-uuid)) 
              name
              address
              nid)))

(defrecord Network [uid name type address mask nid]
  Network-Object
  (common-form [object]
    ""
    (let [addr (get object :address)
          msk (get object :mask)]
      (vector (->Common-Network addr
                                (bit-or addr (bit-xor msk ip-max))))))
  Matchable
  (match? [this that]
    (and (= (get this :type) (get that :type))
         (= (get this :address) (get that :address))
         (= (get this :mask) (get that :mask))))
  store/Serializable
  (serialize [object]
    object)
  store/De-Serializable
  (de-serialize [object store]
    object))

(defn make-network
  ([uid name address mask nid]
   (let [addr-formatted (bit-and address long-mask)
         mask-formatted (bit-and mask long-mask)]
     (->Network (keyword uid)
                name
                :network
                (bit-and addr-formatted mask-formatted)
                mask-formatted
                nid)))
  ([name address mask nid]
     (make-network (keyword (make-uuid))
                   name
                   address
                   mask
                   nid)))

(defrecord Range [uid name type start-addr end-addr nid]
  Network-Object
  (common-form [object]
    (vector (->Common-Network (get object :start-addr)
                              (get object :end-addr))))
  Matchable
  (match? [this that]
    (and (= (get this :type) (get that :type))
         (= (get this :start-addr) (get that :start-addr))
         (= (get this :end-addr) (get that :end-addr))))
  store/Serializable
  (serialize [object]
    object)
  store/De-Serializable
  (de-serialize [object store]
    object))

(defn make-range
  ([uid name start end nid]
   (->Range (keyword uid)
            name
            :range
            start
            end
            nid))
  ([name start end nid]
   (make-range (keyword (make-uuid))
               name
               start
               end
               nid)))
  
(def tcp 6)
(def udp 17)
(def icmp 1)

(defrecord Service [uid name type proto port nid]
  Service-Object
  (common-svc-form [object]
    (vector
     (->Common-Service (get object :proto)
                       (get object :port)
                       (get object :port))))
  Matchable
  (match? [this that]
    (and (= (get this :type) (get that :type))
         (= (get this :proto) (get that :proto))
         (= (get this :port) (get that :port))))
  store/Serializable
  (serialize [object]
    object)
  store/De-Serializable
  (de-serialize [object store]
    object))

(defn make-service
  ([uid name proto port nid]
   (->Service (keyword uid)
              name
              :service
              proto
              port
              nid))
  ([name proto port nid]
   (make-service (keyword (make-uuid))
                 name
                 proto
                 port
                 nid)))

(defrecord Service-Range [uid svc-name type proto start end nid]
  Service-Object
  (common-svc-form [object]
    (vector (->Common-Service (get object :proto)
                              (get object :start)
                              (get object :end))))
  Matchable
  (match? [this that]
    (and (= (get this :type) (get that :type))
         (= (get this :proto) (get that :proto))
         (= (get this :start) (get that :start))
         (= (get this :end) (get that :end))))
  store/Serializable
  (serialize [object]
    object)
  store/De-Serializable
  (de-serialize [object store]
    object))

(defn make-service-range
  ([uid name proto start end nid]
   (->Service-Range (keyword uid)
                    name
                    :service-range
                    proto
                    start
                    end
                    nid))
  ([name proto start end nid]
   (make-service-range (keyword (make-uuid))
                       name
                       proto
                       start
                       end
                       nid)))

(defn is-network?
  [object]
  (let [type (get object :type)]
        (or (= type :host)
            (= type :network)
            (= type :range)
            (if (= type :group)
              (= (get object :group-type) :network)))))

(defn is-service?
  [object]
  (let [type (get object :type)]
        (or (= type :service)
            (= type :service-range)
            (if (= type :group)
              (= (get object :group-type) :service)))))

(defrecord Group [uid name type group-type members nid]
  Network-Object
  (common-form [object]
    (if (is-network? object)
      (vec (flatten (map common-form (get object :members))))))
  Service-Object
  (common-svc-form [object]
    (if (is-service? object)
      (vec (flatten (map common-svc-form (get object :members))))))
  Matchable
  (match? [this that]
    (and (= (get this :type) (get that :type))
         (= (get this :name) (get that :name))
         (= (get this :group-type) (get that :group-type))
         (= (set (get this :members)) (set (get that :members)))))
  store/Serializable
  (serialize [object]
    (->Group (get object :uid)
             (get object :name)
             (get object :type)
             (get object :group-type)
             (vec (map (fn [member] (keyword (get member :uid))) (get object :members)))
             (get object :nid)))
  store/De-Serializable
  (de-serialize [object store]
    (->Group (get object :uid)
             (get object :name)
             (get object :type)
             (get object :group-type)
             (map #(store/get-by-uid store %) (get object :members))  
             (get object :nid))))

(defn make-group
  ([uid group-name group-type members nid]
   (->Group (keyword uid)
            group-name
            :group
            (keyword group-type)
            members
            nid))
  ([group-name group-type members nid]
   (make-group (keyword (make-uuid))
               group-name
               group-type
               members
               nid)))

(defn add-group-member
  [group new-member]
  (make-group
   (get group :uid)
   (get group :name)
   (get group :group-type)
   (cond
     (and (= (get group :group-type) :network)
          (is-network? new-member))
     (conj (get group :members) new-member)
     (and (= (get group :group-type) :service)
          (is-service? new-member))
     (conj (get group :members) new-member)
     :else (get group :members))
   (get group :nid)))

(defrecord Rule [uid type src dst svc action nid]
  Network-Object
  (common-form [object]
    (vec (flatten (conj (common-form (get object :src)) (common-form (get object :dst))))))
  Service-Object
  (common-svc-form [object]
    (vec (common-svc-form (get object :svc))))
  Matchable
  (match? [this that]
    (and (= (get this :type) (get that :type))
         (= (get this :action) (get that :type))
         (match? (get this :src) (get that :src))
         (match? (get this :dst) (get that :dst))
         (match? (get this :svc) (get that :svc))))
  store/Serializable
  (serialize [object]
    (->Rule (get object :uid)
            (get object :type)
            (store/serialize (get object :src))
            (store/serialize (get object :dst))
            (store/serialize (get object :svc))
            (get object :action)
            (get object :nid)))
  store/De-Serializable
  (de-serialize [object store]
    (->Rule (get object :uid)
            (get object :type)
            (store/de-serialize (get object :src) store)
            (store/de-serialize (get object :dst) store)
            (store/de-serialize (get object :svc) store)
            (get object :action)
            (get object :nid))))

(defn make-rule
  ([uid src dst svc action nid]
   (->Rule (keyword uid)
           :rule
           src
           dst
           svc
           action
           nid))
  ([src dst svc action nid]
   (make-rule (keyword (make-uuid))
              src
              dst
              svc
              action
              nid)))

(defn in-src [rule]
  (get rule :src))

(defn in-dst [rule]
  (get rule :dst))

(defn is-rule? [r]
  (= (get r :type) :rule))

(defn is-commonable? [x]
  (or (is-network? x)
      (is-rule? x)))

(defn contains-network?
  [this that]
  (if (and
       (is-commonable? this)
       (is-commonable? that))
    (let [this-common (common-form this)
          that-common (common-form that)]
      (clojure.set/subset?
       #{true}
       (set
        (map (fn [curr-this]
               (clojure.set/subset?
                #{true}
                (set
                 (map
                  (fn [curr-that]
                    (and
                     (or
                      (< (get curr-this :start) (get curr-that :start))
                      (= (get curr-this :start) (get curr-that :start)))
                     (or
                      (> (get curr-this :end) (get curr-that :end))
                      (= (get curr-this :end) (get curr-that :end)))))
                  that-common))))
             this-common))))))

(defn is-svc-commonable? [x]
  (or (is-service? x)
      (is-rule? x)))

(defn contains-service?
  [this that]
  (if (and (is-svc-commonable? this) (is-svc-commonable? that))
    (let [this-common (common-svc-form this)
          that-common (common-svc-form that)]
      (clojure.set/subset?
       #{true}
       (set
        (map (fn [curr-this]
               (clojure.set/subset?
                #{true}
                (set
                 (map
                  (fn [curr-that]
                    (and
                     (or
                      (< (get curr-this :start) (get curr-that :start))
                      (= (get curr-this :start) (get curr-that :start)))
                     (or 
                      (> (get curr-this :end) (get curr-that :end))
                      (= (get curr-this :end) (get curr-that :end)))
                     (= (get curr-this :proto) (get curr-that :proto))))
                  that-common))))
             this-common))))))


(defn str->int
  [x]
  (let [n (read-string x)]
    (if (number? n) n nil)))

(defn combine-addr
  [address total]
  (if (empty? address)
    (bit-shift-right total 8)
    (recur (rest address) (bit-shift-left (bit-or (first address) total) 8))))

(defn convert-ip
  [address]
  (let [parts (map str->int (string/split address #"\."))]
    (combine-addr parts 0)))
