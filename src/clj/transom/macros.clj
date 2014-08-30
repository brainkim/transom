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
