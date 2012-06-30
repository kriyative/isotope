(ns isotope.test.core
  (:use [clojure.test])
  (:require [isotope.core :as core]))

(deftest vars
  (is (= "isotope.core/var-name" (core/var-name #'isotope.core/var-name)))
  (is (= 'isotope.core/var-name (core/sym-with-ns #'isotope.core/var-name)))
  (is (= 'var-name (core/sym #'isotope.core/var-name)))
  )

(deftest var-compare
  (is (= 3 (core/var-compare #'isotope.core/var-name #'isotope.core/sym)))
  (is (= 0 (core/var-compare #'isotope.core/var-name #'isotope.core/var-name)))
  (is (= 1 (core/var-compare #'isotope.core/var-name nil)))
  )

(deftest defn?
  (is (true? (core/defn? #'isotope.core/trace)))
  (is (false? (core/defn? #'isotope.core/*trace-debug*)))
  )

(deftest tracers
  (core/reset-tracers)
  (is (= #{#'println} (core/add-tracer #'println)))
  (is (= #{} (core/remove-tracer #'println)))
  )

(deftest trace
  (let [count (atom 0)
        tracer (fn [v args] (swap! count inc))]
    (try
      (core/add-tracer tracer)
      (core/trace #'isotope.core/defn?)
      (core/defn? #'isotope.core/ns-vars)
      (core/defn? #'isotope.core/ns-vars)
      (is (= 2 @count))
      (finally
        (core/untrace #'isotope.core/defn?)
        (core/remove-tracer tracer))))

  )
