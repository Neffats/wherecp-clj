(ns wherecp.core
  (:gen-class)
  (:require [wherecp.models :as models]
            [wherecp.store :as store]
            [clojure.string :as str]
            [clj-http.client :as client]
            [cheshire.core :as json]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args])

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
  (let [parts (map str->int (str/split address #"\."))]
    (combine-addr parts 0)))


(def test-host
  (models/make-host "test"
             (convert-ip "192.168.1.5")
             "test"))

(def test-net
  (models/make-network "test"
                (convert-ip "192.168.1.0")
                (convert-ip "255.255.255.0")
                "test"))
(def test-range
  (models/make-range "test"
              (convert-ip "192.168.1.2")
              (convert-ip "192.168.1.100")
              "test"))

(def test-http
  (models/make-service "http" models/tcp 80 "test"))

(def test-tcp-80-90
  (models/make-service-range "tcp_80-90" models/tcp 80 90 "test"))

(def test-group (models/make-group "test" "network" (list test-host test-range) "test"))
(def test-svc-grp (models/make-group "50"
                                     "test-svc-grp"
                                     (list test-http
                                           test-tcp-80-90)
                                     "test"))


(def test-group-2 (models/make-group "test" "network" (list test-net test-range) "test"))
(def test-store (store/make-store))
(store/insert test-store test-group-2)
(store/insert test-store test-group)
(store/insert test-store test-host)
(store/insert test-store test-range)
(store/insert test-store test-net)
(store/insert test-store test-http)
(store/insert test-store test-tcp-80-90)

;;(dotimes [n 1000000]
;;  (store/insert test-store
;;                (models/make-host "host"
;;                                  (+ (convert-ip "1.1.1.1") n)
;;                                  "test")))

(def test-rule (models/make-rule
                (models/make-group "src"
                                   "network"
                                   (list test-host)
                                   "test")
                (models/make-group "dst"
                                   "network"
                                   (list test-net)
                                   "test")
                (models/make-group "svc"
                                   "service"
                                   (list test-http)
                                   "test")
                true
                "test-rule"))

(store/insert test-store test-rule)

(defn test-filter
  [store]
  (store/get-by store
                #(models/match? % test-host)))
