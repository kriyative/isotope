(ns isotope.cover
  "A minimal code coverage facility"
  (:require
   [clojure.set :as set]
   [clojure.string :as string]
   [hiccup.core :as h]
   [isotope.core :as core]))

(def ^{:dynamic true} *trace-stats* (atom {}))

(defn coverage-tracer [v args]
  (let [the-ns (ns-name (.ns v))
        fname (core/sym-with-ns v)
        ns-map (or (get @*trace-stats* the-ns) {})]
    (swap! *trace-stats*
           assoc the-ns (assoc ns-map
                          fname (inc (or (get ns-map fname) 0))))))

(defn print-list [l]
  (letfn [(strlen [s] (count (str s)))]
    (doseq [line (reduce (fn [acc item]
                           (if (< 72
                                  (+ (apply + (map strlen (last acc)))
                                     (count (last acc)) ; separators
                                     (strlen item)))
                             (conj acc [item])
                             (conj (vec (butlast acc)) (conj (last acc) item))))
                         [[]]
                         l)]
      (println " " (string/join " " line)))))

(defn summarize [stats]
  (reduce (fn [summary [the-ns smap]]
            (let [ns-fns (core/ns-fns the-ns)
                  evaluated-fns (map resolve (keys smap))
                  diffs (set/difference (set ns-fns) (set evaluated-fns))
                  unevaluated-fns (sort
                                   (map core/sym-with-ns
                                        (filter #(not (nil? %)) diffs)))]
              (conj summary
                    [the-ns {:stats smap
                             :fns-count (count ns-fns)
                             :unevaluated-fns unevaluated-fns}])))
          []
          (sort-by key compare stats)))

(defn text-report [stats]
  (let [summary (summarize stats)]
    (doseq [[the-ns ns-summary] summary]
      (let [stats (:stats ns-summary)
            evaluated-fns-count (count stats)
            unevaluated-fns (:unevaluated-fns ns-summary)
            fns-count (:fns-count ns-summary)]
        (println
         (format "%-50s %d%% %d/%d"
                 (str the-ns ":")
                 (int (* 100 (double (/ evaluated-fns-count fns-count))))
                 evaluated-fns-count
                 fns-count))
        (when-let [doc (:doc (meta the-ns))]
          (prn doc))
        (doseq [[f c] (sort-by second > stats)]
          (println (format "  %-48s %d" (name f) c)))
        (newline)
        (when (not-empty unevaluated-fns)
          (println "  ;;; unevaluated functions")
          (print-list (sort (map name unevaluated-fns)))
          (newline))))
    (let [total-fns-count (reduce + 0 (map #(:fns-count (second %)) summary))
          evaluated-fns-count (reduce + 0 (map #(count (:stats (second %))) summary))]
      (println
       (format "\nOverall coverage: %d%% %d/%d\n"
               (int (* 100 (/ evaluated-fns-count total-fns-count)))
               evaluated-fns-count
               total-fns-count)))))

(defn html-report-head []
  [:head
   [:style {:type "text/css"}
    "tr,td {border: solid 1 px #ccc;}
         td { padding: 4px; }
         .right { text-align: right; }
         .indicator {width: 16px;}
         .narrow { width: 100px; }
         .war { background-color: red; }
         .turmoil { background-color: orange; }
         .restless { background-color: yellow; }
         .peace { background-color: green; }
         .detail { display: none; }
         .evaluated-fns { margin-top: 1em; }
         .unevaluated-fns { margin-top: 1em; }"]
   [:script {:src "http://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js"}]
   [:script {:type "text/javascript"}
    (string/join "\n"
                 ["$(document).ready(function () {"
                  "  $('tr').click(function (e) {"
                  "    $(this).find('.detail').toggle();"
                  "  });"
                  "});"])]])

(defn html-report-coverage-indicator-class [coverage]
  (cond
    (< coverage 0.25) "indicator war"
    (< coverage 0.75) "indicator turmoil"
    (< coverage 1) "indicator restless"
    :else "indicator peace"))

(defn html-report-namespace-summary [summary]
  (vec
   (map (fn [[the-ns ns-summary]]
          (let [stats (:stats ns-summary)
                evaluated-fns-count (count stats)
                unevaluated-fns (:unevaluated-fns ns-summary)
                fns-count (:fns-count ns-summary)
                coverage (double (/ evaluated-fns-count fns-count))]
            [:tr {:valign :top}
             [:td {:class (html-report-coverage-indicator-class coverage) :width "10"}]
             [:td {:class :description}
              (str the-ns)
              [:div {:class :detail}
               (concat
                [[:ul {:class :evaluated-fns}]]
                (vec
                 (map (fn [[f c]] [:li (str (name f) " (" c ")")])
                      (sort-by second > stats))))
               (when (not-empty unevaluated-fns)
                 [:div {:class :unevaluated-fns}
                  [:div [:b "unevaluated functions:"]]
                  (string/join ", " (sort (map name unevaluated-fns)))])]]
             [:td {:class "narrow right"} (str (int (* 100 coverage)) "%")]
             [:td {:class "narrow right"}
              (str evaluated-fns-count "/" fns-count)]]))
        summary)))

(defn html-report-coverage-summary [summary]
  (let [total-fns-count (reduce + 0 (map #(:fns-count (second %)) summary))
        evaluated-fns-count (reduce + 0
                                    (map #(count (:stats (second %))) summary))
        coverage (/ evaluated-fns-count total-fns-count)]
    [:tr {:valign :top}
     [:td {:class (html-report-coverage-indicator-class coverage)}]
     [:td "Overall coverage"]
     [:td {:class "narrow right"} (str (int (* 100 coverage)) "%")]
     [:td {:class "narrow right"}
      (str evaluated-fns-count "/" total-fns-count)]]))

(defn html-report [stats]
  (let [summary (summarize stats)]
    (h/html
     [:html
      (html-report-head)
      [:body
       `[:table {:cellspacing 0 :width "100%"}
         ~@(html-report-namespace-summary summary)
         ~(html-report-coverage-summary summary)]]])))

(def ^{:dynamic true} *trace-reporter* nil)

(defn trace-report [] ((or *trace-reporter* text-report) @*trace-stats*))

(defmacro with-coverage [{:keys [reporter]} & body]
  `(binding [*trace-stats* (atom {})
             *trace-reporter* ~reporter]
     (try
       (core/add-tracer coverage-tracer)
       (let [val# (do ~@body)]
         (trace-report)
         val#)
       (catch Exception ex#
         (core/remove-tracer coverage-tracer)
         (throw ex#)))))

(defmacro do-coverage [& body]
  `(binding [*trace-stats* (atom {})]
     ~@body
     @*trace-stats*))
