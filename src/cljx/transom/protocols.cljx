(ns transom.protocols)

(defprotocol Patchable
  (patch [this edit]))

(defprotocol Diffable
  (diff [this that]))

(defprotocol WithRebasableRef
  (rebase-ref [this key edit destructive?]))

(defprotocol WithComposableEdit
  (compose [this old new]))

(defprotocol WithTransformableEdit
  (transform [this mine yours]))
