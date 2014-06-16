(ns transom.protocols)

(defprotocol Patchable
  (patch [this edit]))

(defprotocol Diffable
  (diff [this that]))

(defprotocol WithRebasableKey
  (rebase-key [this key edit destructive?]))

(defprotocol WithTransformableEdit
  (transform [this edit1 edit2]))

(defprotocol WithComposableEdit
  (compose [this edit1 edit2]))
