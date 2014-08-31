(ns transom.document
  (:require [transom.core :as transom]))

(defprotocol IDocument
  (value [this])
  (version [this])
  (transform-edit [this edit last-seen])
  (patch [this edit]))

(defrecord Document [value history]
  IDocument
  (value [this] value)
  (version [this] (count history))
  (transform-edit [this edit last-seen]
    (if (< last-seen (version this))
      (reduce (fn [yours mine] (second (transom/transform value mine yours)))
              edit (subvec history last-seen))
      edit))
  (patch [this edit]
    (let [value (transom/patch value edit), history (conj history edit)]
      (Document. value history))))

(defn document
  ([]
    (document ""))
  ([initial]
    (document initial []))
  ([current history]
    (->Document current history)))
