(ns clojeers.core
  (:import (java.io InputStreamReader BufferedReader OutputStreamWriter)
           (java.net ServerSocket Socket BindException SocketException)
           (java.util Date))
  (:require [clojure.java.io :as io]
            [clojure.core.async :refer [go-loop]]
            [clojure.string :refer [split]])
  )


(defn sread-line [in]
  (binding [*in* in]
    (read-line)))
(defn sprintln [out & rest]
  (binding [*out* out]
    (apply println rest)
    (flush)))
(defn sprint [out & rest]
  (binding [*out* out]
    (apply print rest)
    (flush)))
(defn sprn [out & rest]
  (binding [*out* out]
    (apply println rest)
    (flush)))
(defn spr [out & rest]
  (binding [*out* out]
    (apply print rest)
    (flush)))

(defn parse [in]
  (let [in (clojure.string/trim in)
        [in last] (split in #" :" 2)
        items (split in #"\s")
        items (conj items last)
        items (apply list (filter not-empty items))
        [prefix items] (if (= (first (first items)) \:) [(first items) (pop items)] [nil items])
        command (first items)
        items (vec (pop items))
        ]
    {:prefix  prefix
     :command (-> command clojure.string/lower-case keyword)
     :args    items
     })
  )

(defn respond [{:keys [prefix command args]}]
  (apply str prefix " " (-> command clojure.string/upper-case name) " " args)
  )

(defmulti execute :command)

(defmethod execute :ping [self state & {:keys [client] :as data}]
  (swap! state #(assoc % :ping (Date.)))

  {:prefix ":server"
   :command :pong
   :args [(-> self :args first)]
   }
  )


(defn ^ServerSocket create-server [port handler]
  (try (let [server (ServerSocket. port)]
         (println "Server started")
         (go-loop []
           (if-let [_ (try (let [client (.accept server)]
                             (println "Accepted connection" client)
                             (let [in (io/reader client)
                                   out (io/writer client)]
                               (go-loop [continue? true]
                                 (if continue?
                                   (recur
                                     (if-let [res (try
                                                    (handler in out {:client client})
                                                    (catch SocketException e
                                                      (println "Closed connection " client))
                                                    (catch Exception e
                                                      (prn e)))]
                                       res
                                       false)))
                                 ))
                             true)
                           (catch SocketException e
                             (println "Server stopped")))]
             (recur)))
         server)
       (catch BindException e
         (println "Cannot bind port"))
       (catch SocketException e
         (println "Server stopped"))
       (catch Exception e
         (prn e))))


(def clients (ref {}))
(def state (ref {}))

(defn echo [in out & {:keys [client] :as data}]
  #_(binding [*in* in
              *out* out]
      (let [input (read-line)]
        (println input)
        (flush))
      )
  (if-let [input (sread-line in)]
    (do
      (let [action (parse input)]
        (sprn out (respond (apply execute action state data)))
        )
      true)
    (do
      (println (str "Client closed connection: " client))
      false)
    )
  )

(defn echo-server []
  (create-server 5556 echo))

(defn stop-server [server]
  (.close server))

(comment
  (stop-server my-server)
  (def my-server (echo-server))
  )
