(ns transom.examples.rooms
  (:require [clojure.core.async :as async :refer [>! <!]]
            [clojure.tools.reader.edn :as edn]
            [org.httpkit.server :as server]
            [transom.document :as document]
            [com.stuartsierra.component :as component]))

(def !ids (async/to-chan (range)))

(defn send-all!
  [chans message]
  (doseq [chan chans]
    (server/send! chan message)))

(defn room
  [input killer]
  (let [pub (async/pub input #(get-in % [0 :type]))
        inits (async/sub pub :init (async/chan))
        edits (async/sub pub :edit (async/chan))
        closes (async/sub pub :close (async/chan))]
    (async/go-loop [doc (document/document) chans #{}]
      (async/alt!
        inits
        ([[_ sender]]
          (let [message (pr-str {:type :init
                                 :id (<! !ids)
                                 :doc (document/value doc)
                                 :version (document/version doc)})]
            (server/send! sender message)
            (recur doc (conj chans sender))))
        edits
        ([[message sender]]
          (let [{:keys [edit version id]} message
                edit' (document/transform-edit doc edit version)
                doc' (document/patch doc edit')
                version' (document/version doc')]
            (send-all! chans (pr-str {:type :edit
                                      :edit edit'
                                      :version version'
                                      :id id}))
            (recur doc' chans)))
        closes
        ([[_ sender]]
          (recur doc (disj chans sender)))
        killer
        ([_]
          (println "killing room"))))
    input))

(defrecord Rooms [killer]
  component/Lifecycle
  (start [this]
    (let [textarea (async/chan)
          killer (async/chan)]
      (merge this {:textarea (room textarea killer)
                   :killer killer})))
  (stop [this]
    (async/put! killer :killllitttt)
    this))

(defn rooms
  []
  (map->Rooms {}))
