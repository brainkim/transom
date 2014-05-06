(ns transom.core
  (:require
      #+clj [clojure.core.match :refer [match]]
     #+cljs [cljs.core.match])
  #+cljs
  (:require-macros [cljs.core.match.macros :refer [match]]))
