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
  ([input]
    (room input (document/document)))
  ([input doc]
    (room input doc #{}))
  ([input doc chans]
    (async/go
      (when-let [[message sender] (<! input)]
        (case (message :type)
          :init
          (do
            (server/send! sender (pr-str {:type :init
                                          :id (<! !ids)
                                          :doc (document/value doc)
                                          :version (document/version doc)}))
            (room input doc (conj chans sender)))
          :edit
          (let [{:keys [edit version id]} message
                edit' (document/transform-edit doc edit version)
                doc' (document/patch doc edit')
                version' (document/version doc')]
            (send-all! chans (pr-str {:type :edit
                                      :id id
                                      :edit edit'
                                      :version version'}))
            (room input doc' chans))
          :close
          (room input doc (disj chans sender)))))
    input))

(defrecord Rooms []
  component/Lifecycle
  (start [this]
    (merge this {:textarea (room (async/chan))}))
  (stop [this]
    this))

(defn rooms
  []
  (map->Rooms {}))
