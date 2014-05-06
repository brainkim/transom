(ns transom.protocols)

(defprotocol EditRebase
  (rebase [this edit1 edit2])
  (rebase-path [this edit path]))

(defprotocol EditCompose
  (compose [this edit1 edit2])
  (compose-path [this edit path]))
