(ns isotope.core
  "A pluggable tracer facility"
  (:require
   [clojure.string :as string])
  (:import clojure.lang.Var))

(def ^{:dynamic true} *trace-debug* false)

(defn var-name
  "Namespace qualified name for the specified var."
  [^Var v]
  (when v (str (.ns v) "/" (.sym v))))

(defn sym-with-ns
  "Namespace qualified symbol for the specified var."
  [^Var v]
  (when-let [name (var-name v)]
    (symbol name)))

(defn sym [^Var v] (when v (.sym v)))

(defn var-compare [v1 v2] (compare (sym-with-ns v1) (sym-with-ns v2)))

(defn defn? [^Var v] (and (fn? @v) (not (:macro (meta v)))))

(defn to-ns [the-ns] (if (symbol? the-ns) (find-ns the-ns) the-ns))

(defn ns-vars [the-ns] (vals (ns-interns (to-ns the-ns))))

(defn ns-fns [the-ns] (filter defn? (ns-vars the-ns)))

(def ^{:private true} tracers (atom #{}))

(defn reset-tracers [] (reset! tracers #{}))

(defn add-tracer
  "Register a tracer function of the form (fn [var args] ...) to be
  run on traced functions."
  [tracer]
  (swap! tracers conj tracer))

(defn remove-tracer
  "Remove a previously registered tracer function."
  [tracer]
  (reset! tracers (set (remove #{tracer} @tracers))))

(defn trace
  "Trace the specfied vars/symbols. Skips any vars that are already
  being traced."
  [& vars]
  (doseq [v vars]
    (let [^Var v (if (var? v) v (resolve v))]
      (when (and (defn? v) (not (::pretrace (meta v))))
        (let [the-ns (.ns v)
              fname (sym-with-ns v)]
          (alter-meta! v assoc ::pretrace @v)
          (alter-var-root v
                          (fn [f]
                            (fn [& args]
                              (doseq [tracer @tracers]
                                (try
                                  (tracer v args)
                                  (catch Exception ex
                                    (println ";;; tracer exception:" ex))))
                              (apply f args)))))))))

(defn untrace
  "Untrace specified var(s)"
  [& vars]
  (doseq [v vars]
    (let [v (if (var? v) v (resolve v))]
      (when-let [f (::pretrace (meta v))]
        (alter-var-root v (constantly f))
        (alter-meta! v dissoc ::pretrace)))))

(defn trace-ns
  "Trace all the top level functions in specified namespace."
  [the-ns]
  (doseq [v (ns-vars the-ns)] (trace v)))

(defn untrace-ns
  "Untrace all the top level functions in specified namespace."
  [the-ns]
  (doseq [v (ns-vars the-ns)] (untrace v)))
