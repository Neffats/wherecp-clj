(ns wherecp.store
  (:gen-class))

(defprotocol Serializable
  (serialize [object]))
  
(defprotocol De-Serializable
  (de-serialize [object store]))
  
(defn make-store
  []
  (atom (hash-map)))

(defn insert
  [store object]
  (swap! store assoc (keyword (get object :uid)) (serialize object)))
  
(defn get-by-uid
  [store uid]
  (de-serialize (get @store (keyword uid)) store))

(defn get-by-name
  [store name]
  (map #(de-serialize % store)
       (filter
        #(= name (get % :name))
        (map val @store))))

(defn get-by
  [store pred]
  (filter pred (map #(de-serialize % store) (map val @store))))
