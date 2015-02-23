(ns transom.macros)

(defn ^:private emit-expanded-specs
  [specs]
  (apply concat
    (for [[ts spec] (partition 2 specs)
          t ts]
      (list t spec))))

(defmacro extend-protocols
  [p & specs]
  `(extend-protocol ~p ~@(emit-expanded-specs specs)))

;; Adapted from http://dev.clojure.org/jira/browse/CLJ-415 WHY NOT HICKEY?
(defn ^{:private true} local-bindings
  "Produces a map of the names of local bindings to their values."
  [env]
  (let [symbols (map key env)]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))

(defmacro assert*
  [x]
  (when *assert*
    (let [bindings (local-bindings &env)]
      `(when-not ~x
         (let [sep# \newline
               form# '~x]
           (throw (AssertionError. (apply str "Assert failed: " (pr-str form#) sep#
                                          (map (fn [[k# v#]] 
                                                 (when (some #{k#} (flatten form#)) 
                                                   (str "\t" k# " : " v# sep#))) 
                                               ~bindings)))))))))
