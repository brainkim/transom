(ns transom.protocols)

(defprotocol Patchable
  (patch [this edit]))

(defprotocol Diffable
  (diff [this that]))

(defprotocol WithRebaseableEdit
  (rebase [this edit1 edit2]))

(defprotocol WithRebaseableKey
  (rebase-key [this key edit destructive?]))

(defprotocol WithComposableEdit
  (compose [this edit1 edit2]))
