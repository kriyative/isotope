(ns isotope.test.cover
  (:use [clojure.test])
  (:require
   [isotope.core :as core]
   [isotope.cover :as cover]))

(deftest cover

  (is (= {'isotope.core {'isotope.core/defn? 2}}
         (cover/do-coverage
           (core/reset-tracers)
           (core/trace #'isotope.core/defn?)
           (core/add-tracer cover/coverage-tracer)
           (core/defn? #'isotope.core/ns-vars)
           (core/defn? #'isotope.core/ns-vars)
           (core/untrace #'isotope.core/defn?)
           (core/remove-tracer cover/coverage-tracer))))

    (is (= {'isotope.cover {'isotope.cover/print-list 1}
            'isotope.core {'isotope.core/sym 1 'isotope.core/defn? 2}}
           (let [vars [#'isotope.core/defn?
                       #'isotope.core/sym
                       #'isotope.cover/print-list]]
             (cover/do-coverage
               (core/reset-tracers)
               (apply core/trace vars)
               (core/add-tracer cover/coverage-tracer)
               (core/defn? #'isotope.core/ns-vars)
               (core/defn? #'isotope.core/ns-vars)
               (core/sym #'isotope.core/ns-vars)
               (with-out-str (cover/print-list ["a" "b" "c"]))
               (apply core/untrace vars)
               (core/remove-tracer cover/coverage-tracer)))))
  
  (let [unevaluated-fns (sort (remove #{'isotope.core/defn?}
                                      (map core/sym-with-ns
                                           (core/ns-fns 'isotope.core))))]
    (is (= [['isotope.core
             {:stats {'isotope.core/defn? 2}
              :fns-count (+ 1 (count unevaluated-fns))
              :unevaluated-fns unevaluated-fns}]]
           (cover/summarize {'isotope.core {'isotope.core/defn? 2}}))))

  )
