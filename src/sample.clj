(ns sample
  (:require [sicmutils.series :as s]
            [sicmutils.value :refer :all]
            [sicmutils.generic :refer :all]
            [sicmutils.expression.render :refer :all]
            [clojure.string]
            [clojure.algo.generic.functor :refer :all]))

;;;; Helpers

(defn render* [v]
  (binding [*TeX-sans-serif-symbols* false
            *TeX-vertical-down-tuples* false]
    (->TeX v)))

(defn render [v]
  (str "$" (render* v) "$"))

(defn align
  ([eq]
   (str "\\begin{align*}"
        (-> (render* eq)
            (clojure.string/replace #"(=|>|<|\\geq|\\leq)" "\\\\\\\\&$1")
            (clojure.string/replace-first #"\\\\&(=|>|<|\\geq|\\leq)" "&$1"))
        "\\end{align*}"))
  ([eq & eqs]
   (str "\\begin{align*}"
        (clojure.string/join
         "\\\\"
         (map (fn [eq]
                (-> (render* eq)
                    (clojure.string/replace #"(=|>|<|\\geq|\\leq)" "\\\\\\\\&$1")
                    (clojure.string/replace-first #"\\\\&(=|>|<|\\geq|\\leq)" "&$1")))
              (into [eq] eqs)))
        "\\end{align*}")))

(deftype Equation [sym args]
  Object
  (toString [this]
    (str (freeze this)))
  sicmutils.value.Value
  (kind [_]
    :sicmutils.expression/numeric)
  (freeze [_]
    `(~sym ~@(map freeze args))))

(defn eq [& args] (Equation. '= args))
(defn gt [& args] (Equation. '> args))
(defn lt [& args] (Equation. '< args))
(defn gte [& args] (Equation. '>= args))
(defn lte [& args] (Equation. '<= args))

;; (defmethod = [:sicmutils.expression/numeric :sicmutils.expression/numeric]
;;   [l r]
;;   (eq l r))
;; (defmethod = [:sicmutils.expression/numeric :sicmutils.value/number]
;;   [l r]
;;   (eq l r))
;; (defmethod = [:sicmutils.value/number :sicmutils.expression/numeric]
;;   [l r]
;;   (eq l r))

(defn draw-cashflow [xs]
  (let [c         (max (count xs) 2)
        h-max     8
        h-spacing #(str "(" (* (float (/ h-max (dec c))) %) ",0)")
        v-spacing #(str "(0," (if (pos? %1) 0.8 -0.8) ")")
        arrow     "\\draw[->]"
        node      #(str "node[" (if (pos? %1) "above" "below") "]{" (render %2) "}")
        arrows    (->> xs
                       (map-indexed
                        (fn [idx [n [in out]]]
                          [(when in
                             (str arrow
                                  (h-spacing idx)
                                  (node -1 n)
                                  " -- ++"
                                  (v-spacing 1)
                                  (node 1 in)))
                           (when out
                             (str arrow
                                  (h-spacing idx)
                                  (node 1 n)
                                  " -- ++"
                                  (v-spacing -1)
                                  (node -1 out)))
                           (when (and (not in) (not out))
                             (str "\\draw[-]"
                                  (h-spacing idx)
                                  (node -1 n)))]))
                       (apply concat))]
    (str "\\begin{center}\n\\begin{tikzpicture}\n\\draw[-](0,0) -- (" h-max ",0);\n"
         (clojure.string/join ";\n" (remove nil? arrows))
         "\n\\end{tikzpicture}\n\\end{center}\n")))

(comment
  (draw-cashflow {0 [10 -10]}))

(deftype CashFlow [xs]
  Object
  (toString [this]
    (str (freeze this)))
  sicmutils.value.Value
  (kind [_]
    (kind xs))
  (freeze [_]
    xs))

(defn cashflow [xs]
  (CashFlow. xs))

;;;; Interest

(defn simple [i]
  (fn [n]
    (+ 1 (* i n))))

(defn compound [i]
  (fn [n]
    (expt (+ 1 i) n)))

(defn compound-index [is]
  (fn [n]
    (if (symbol? n)
      (reduce * (mapv #(+ 1 %) is))
      (reduce * (map #(+ 1 %) (take n is))))))

(comment
  ((simple 'i) 'n)
  ((compound 'i) 'n)

  ((simple 0.1) 12)
  ((compound 0.1) 12)
  ((compound 0.1) -12)

  ((compound-index [0.3 0.5 0.6]) 3))

(defn pv [f n m]
  (/ m (f n)))

(defn fv [f n m]
  (* m (f n)))

(defn interest [f n m]
  (* m (- (f n) 1)))

(defn rate
  ([factor]
   (rate factor 1))
  ([fv pv]
   (- (/ fv pv) 1)))

(comment
  (fv (simple 'i) 'n 'pv)
  (pv (simple 'i) 'n 'fv)
  (fv (compound 'i) 'n 'pv)
  (pv (compound 'i) 'n 'fv)

  (fv (simple 0.1M) 12 1000)
  (pv (simple 0.1M) 12 2200)
  (fv (compound 0.1M) 12 1000)
  (pv (compound 0.1M) 12 3138.428376721000M))

(def y->m 1/12)
(def m->y 12)
(def m->d252 1/21)
(def m->d360 1/30)
(def m->d365 12/365)
(def m->dActual (/ 12 (/ (+ (* 303 365) (* 97 366)) 400))) ; In every 400-year period, there are 303 regular years and 97 leap years in the Gregorian calendar.

(comment
  ((compound 0.1) (* 21 m->d252))

  ((compound 0.1) (* 30 m->d360))

  ((compound 0.1) (* 30 m->d365))
  ((compound 0.1) (* 31 m->d365))
  ((compound 0.1) (* (/ 365 12) m->d365))

  ((compound 0.1) (* 30 m->dActual))
  ((compound 0.1) (* 31 m->dActual))
  ((compound 0.1) (* (/ 365.2425 12) m->dActual)))

;;;; Series

(defn map->series [indexed]
  (s/generate #(indexed % 0) :sicmutils.series/series))

(defn const [x]
  (fn [n]
    (if (zero? n) 1 x)))

(defn x->series [x]
  (s/generate (const x) :sicmutils.series/series))

(defn xs->series [xs]
  (apply s/series (into [1] xs)))

(defn term [f]
  (fn [n]
    (if (zero? n) 1
        (- (interest f n 1)
           (interest f (- n 1) 1)))))

(defn i->series [f]
  (s/generate (term f) :sicmutils.series/series))

(defn fn->series [fn]
  (s/generate fn :sicmutils.series/series))

(comment
  (->infix (fn->series #(pv (compound 'i) % (nth [(- 'C_0) 'C_1 'C_2 'C_3 'C_4] %)))))

;;;; Amortization methods

;; Derivation of formula for Price (aka French or Loan-style) amortization method
;;
;; Let $ 1000 @ 10% in 3 payments be represented by the table:
;;
;;   pmt    amort  interest balance
;; 0 -      0      0        1000.00
;; 1 402.11 302.11 100.00   697.89
;; 2 402.11 332.32 69.79    365.57
;; 3 402.11 365.55 36.56    0
;;
;; We use the PMT formula to find the coefficient of equal payments:
;;
;; (* 1000 (pmt (compound 0.1) 3)) => 402.11480362537765
;;
;; Expanding the balance formula for each period gives:
;;
;; (- (* 1000 1.1) 402.11) => 697.89
;; (- (* (- (* 1000 1.1) 402.11) 1.1) 402.11) => 365.5690000000001
;; (- (* (- (* (- (* 1000 1.1) 402.11) 1.1) 402.11) 1.1) 402.11) => 0.0159000000001015

(defn pmt [f n]
  (/ (- (f 1) 1)
     (- 1 (f (- n)))))

(defn price
  ([i n pv]
   (let [i'  (compound i)
         p'  (- (pmt i' n))
         cf  (* pv (if (number? n)
                     (xs->series (repeat n p'))
                     (x->series p')))
         icf (* cf (i->series i'))]
     (with-meta
       {:payments      cf
        :amortizations (- icf (s/series pv))
        :interest      (- icf cf)
        :balance       (s/partial-sums icf)}
       {:i i
        :n n
        :pv pv}))))

(comment
  (price 0.1 3 1000))

;; Derivation of formula for Straight-Line (aka Constant or Mortgage-style) amortization method
;;
;; Let $ 1000 @ 10% in 3 payments be represented by the table:
;;
;;   amort  pmt    interest balance
;; 0 -      -      0        1000.00
;; 1 333.33 433.33 100.00   666.66
;; 2 333.33 400.00 66.66    333.33
;; 3 333.33 366.67 33.33    0
;;
;; Expanding the payment formula for each period gives:
;;
;; (/ (+ (* 1000 3 0.1) 1000) 3) => 433.3333333333333
;; (/ (+ (* 1000 3 0.1) (* -1000 0.1) 1000) 3) => 400.0
;; (/ (+ (* 1000 3 0.1) (* 2 -1000 0.1) 1000) 3) => 366.6666666666667

(defn straight
  ([i n pv]
   (let [i'  (simple i)
         p'  (- (pmt i' n))
         cf  (* pv (if (number? n)
                     (xs->series (repeat n p'))
                     (x->series p')))
         icf (* cf (i->series i'))]
     (with-meta
       {:payments      (- cf (- icf cf))
        :amortizations (- cf (s/series pv))
        :interest      (- icf cf)
        :balance       (s/partial-sums cf)}
       {:i i
        :n n
        :pv pv}))))

(comment
  (straight 0.1 3 1000))

(defn as-table [t headers]
  (let [{:keys [n]} (meta t)
        pivoted (->> t
                     (map (fn [[k v]]
                            (into [(get headers k)] (take (inc n) v))))
                     (apply mapv vector))]
    (into [(first pivoted) nil] (rest pivoted))))

(comment
  (as-table (with-meta {:foo [1 2 3]} {:n 3}) {:foo ""}))

(defn table->cashflow [t]
  (let [p (take (inc (:n (meta t))) (:payments t))]
    (cashflow
     (into {}
           (map-indexed
            (fn [k v]
              (if (pos? v) {k [v nil]} {k [nil v]})) p)))))

;;;; Ledger

(defn balance-sheet [& {:as args}]
  (reduce
   (partial merge-with +)
   (for [[rule amounts] args]
     (let [[credit debit] rule]
       {credit amounts
        debit  (- amounts)}))))

(defn run-sheet [n s]
  (fmap #(s/sum % n) s))

(comment
  (let [t (straight 0.1M 3 1000)]
   (->> (balance-sheet [:pnl/revenue :asset/loan] (:interest t)
                       [:asset/cash :asset/loan] (:payments t))
        (run-sheet 4))))

(comment
  (let [t (price 0.1 3 1000)]
   (->> (balance-sheet [:pnl/revenue :asset/loan] (:interest t)
                       [:asset/cash :asset/loan] (:payments t))
        (run-sheet 4))))
