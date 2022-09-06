(ns clojeers.core
  (:import (java.io InputStreamReader BufferedReader OutputStreamWriter)
           (java.net ServerSocket Socket BindException SocketException)
           )
  (:require [clojure.java.io :as io]
            [clojure.core.async :refer [go-loop]]
            [clojure.string :refer [split]])
  )


(defn parse [in]
  (let [[in last] (split in #" :" 2)
        items (split in #"\s")
        items (conj items last)
        items (vec (filter not-empty items))]
    {:prefix  nil
     :command :privmsg
     :args    items
     })
  )

(defn ^ServerSocket create-server [port handler]
  (try (let [server (ServerSocket. port)]
         (println "Server started")
         (go-loop []
           (if-let [_ (try (let [client (.accept server)]
                             (println "Accepted connection" client)
                             (let [in (io/reader client)
                                   out (io/writer client)]
                               (go-loop []
                                 (if-let [_ (try
                                              (handler in out)
                                              true
                                              (catch SocketException e
                                                (println "Closed connection " client))
                                              (catch Exception e
                                                (prn e)))]
                                   (recur))))
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

(def clients (ref {}))
(def state (ref {}))

(defn echo [in out]
  #_(binding [*in* in
              *out* out]
      (let [input (read-line)]
        (println input)
        (flush))
      )
  (if-let [input (sread-line in)]
    (sprn out (parse input))
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
