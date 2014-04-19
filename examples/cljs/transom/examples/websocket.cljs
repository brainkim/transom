(ns transom.examples.websocket
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [cljs.core.async :as async :refer [<! chan put! close!]]
            [cljs.core.async.impl.protocols :as impl]
            [goog.events :refer [listen]]
            [goog.net.WebSocket.EventType :refer [OPENED CLOSED ERROR MESSAGE]])
  (:import (goog.net WebSocket)))

(defn socket
  [url]
  (let [incoming (chan)
        outgoing (chan)
        sock (goog.net.WebSocket.)]
    (specify! sock
      impl/WritePort
      (put! [_ val handler]
        (impl/put! outgoing val handler))
      impl/ReadPort
      (take! [_ handler]
        (impl/take! incoming handler))
      impl/Channel
      (close! [_]
        (close! incoming)
        (close! outgoing)
        (.close sock)
        (.dispose sock)))

    (listen sock OPENED
      (fn [_]
        (.send sock (pr-str {:type :init}))))
    (listen sock CLOSED
      (fn [_]
        (close! sock)))
    (listen sock ERROR
      (fn [ev]
        (put! incoming (pr-str {:type :error :error ev}))
        (close! sock)))
    (listen sock MESSAGE
      (fn [ev]
        (let [message (.-message ev)]
          (put! incoming message))))

    (go-loop []
      (if-let [message (<! outgoing)]
        (do
          (.send sock message)
          (recur))
        (close! sock)))

    (.open sock url)
    sock))
