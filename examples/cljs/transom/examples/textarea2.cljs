(ns transom.examples.textarea2
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]])
  (:require [cljs.core.async :as async :refer [put! chan >! <! timeout]]
            [cljs.reader :refer [read-string]]
            [transom.core :as transom]
            [transom.diff :as diff]
            [transom.document :as document]
            [goog.dom :as gdom]
            [clojure.browser.event :as event]
            [transom.examples.websocket :refer [socket]]))
(enable-console-print!)

(def ^:dynamic *sock* nil)

(def textarea
  (->> (gdom/createDom "textarea" #js {:style "width: 500px; height: 500px;"})
       (gdom/append (gdom/getElement "app"))))

(defn attach-events
  [elem]
  (letfn [(handler [ev] )]
    (doseq [events []]
      (-> elem
          identity))))

(defn host
  []
  (.. js/window -location -host))
