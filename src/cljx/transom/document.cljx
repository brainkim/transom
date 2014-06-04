(ns transom.document
  (:require [transom.string :as transom]))

(defprotocol IDocument
  (value [this])
  (version [this])
  (transform-edit [this edit] [this edit last-seen])
  (patch [this edit]))

(defrecord Document [value history]
  IDocument
  (value [this] value)
  (version [this] (count history))
  (transform-edit [this edit]
    (transform-edit this edit (version this)))
  (transform-edit [this edit last-seen]
    (if (< last-seen (version this))
      ;; transform always starts with past-edit
      (reduce (fn [their-edit our-edit] (second (transom/transform our-edit their-edit)))
              edit (subvec history last-seen))
      edit))
  (patch [this edit]
    (let [value (transom/patch value edit)
          history (conj history edit)]
      (Document. value history))))

(defn document
  ([]
    (document ""))
  ([initial]
    (document initial []))
  ([current history]
    (->Document current history)))
