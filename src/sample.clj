(ns sample
  (:require [sicmutils.series :as s]
            [sicmutils.value :refer :all]
            [sicmutils.generic :refer :all]
            [sicmutils.expression.render :refer :all]
            [clojure.string]
            [clojure.algo.generic.functor :refer :all]))

;;;; Helpers

(defn render* [v]
  (binding [*TeX-sans-serif-symbols* false]
    (->TeX v)))

(defn render [v]
  (str "$" (render* v) "$"))

(defn align
  ([eq]
   (str "\\begin{align*}"
        (-> (render* eq)
            (clojure.string/replace "=" "\\\\&=")
            (clojure.string/replace-first "\\\\&=" "&="))
        "\\end{align*}"))
  ([eq & eqs]
   (str "\\begin{align}"
        (->> eqs
             (into [eq])
             (map (fn [eq]
                    (-> (render* eq)
                        (clojure.string/replace "=" "\\\\&=")
                        (clojure.string/replace-first "\\\\&=" "&="))))
             (clojure.string/join "\\\\"))
        "\\end{align}")))

(deftype Equation [args]
  Object
  (toString [this]
    (str (freeze this)))
  sicmutils.value.Value
  (kind [_]
    :sicmutils.expression/numeric)
  (freeze [_]
    `(~'= ~@(map freeze args))))

(defn eq [& args]
  (Equation. args))

(defmethod = [:sicmutils.expression/numeric :sicmutils.expression/numeric]
  [l r]
  (eq l r))

(defmethod = [:sicmutils.expression/numeric :sicmutils.value/number]
  [l r]
  (eq l r))

(defmethod = [:sicmutils.value/number :sicmutils.expression/numeric]
  [l r]
  (eq l r))

;;;; Interest

(defn simple [i]
  (fn [n]
    (+ 1 (* i n))))

(defn compound [i]
  (fn [n]
    (expt (+ 1 i) n)))

(comment
  ((simple 'i) 'n)
  ((compound 'i) 'n)

  ((simple 0.1) 12)
  ((compound 0.1) 12)
  ((compound 0.1) -12))

(defn pv [f n m]
  (/ m (f n)))

(defn fv [f n m]
  (* m (f n)))

(defn interest [f n m]
  (* m (- (f n) 1)))

(defn rate [fv pv]
  (- (/ fv pv) 1))

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
  ((compound 'i) (* 'n m->d252))
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

(defn term [f]
  (fn [n]
    (if (zero? n) 1
        (interest f n 1))))

(defn i->series [f]
  (s/generate (term f) :sicmutils.series/series))

(defn compound-index [is]
  (fn [n]
    (if (symbol? n)
      (reduce * (mapv #(+ 1 %) is))
      (reduce * (map #(+ 1 %) (take n is))))))

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
;;
;; Replacing the values and simplifying the formula:
;;
;; (render (- (* (- (* (- (* 'pv 'i) 'pmt) 'i) 'pmt) 'i) 'pmt))
;; => "i³ pv - i² pmt - i pmt - pmt"
;;
;; The general balance formula is:
;;
;; = iᵏ p₀ + i⁽ᵏ⁻¹⁾ p₁ + ... pₖ
;;
;; Where p₀ = present value, and p₁...pₖ = pmt.
;; We can interpret the present value (principal) as a payment w/ inverted sign.

(defn pmt [f n]
  (/ (- (f 1) 1)
     (- 1 (f (- n)))))

(defn price
  ([i n pv]
   (let [i'  (compound i)
         p'  (- (pmt i' n))
         cf  (* pv (x->series p'))
         icf (* cf (i->series i'))]
     {:payments      cf
      :amortizations (- icf (s/series pv))
      :interest      (- icf cf)
      :balance       (s/partial-sums icf)})))

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
;;
;; (render (/ (+ (* 'pv 'n 'i) (* 2 (- 'pv) 'i) 'pv) 'n))
;; => "(-2 i pv + i n pv + pv) /n"
;; 
;; The general formula for the k'th payment is:
;;
;; (-k i pv + i n pv + pv) / n

(defn straight
  ([i n pv]
   (let [i'  (simple i)
         p'  (- (pmt i' n))
         cf  (* pv (x->series p'))
         icf (* cf (i->series i'))]
     {:payments      (- cf (- icf cf))
      :amortizations (- cf (s/series pv))
      :interest      (- icf cf)
      :balance       (s/partial-sums cf)})))

(comment
  (straight 0.1 3 1000))

;;;; Ledger

(defn balance-sheet [& {:as args}]
  (reduce
   (partial merge-with +)
   (for [[rule amounts] args]
     (let [[credit debit] rule]
       {credit amounts
        debit  (- amounts)}))))

(defn run-sheet [n s]
  (fmap #(take n (s/partial-sums %)) s))

(comment
  (let [t (straight 0.1 3 1000)]
   (->> (balance-sheet [:pnl/revenue :asset/loan] (:interest t)
                       [:asset/cash :asset/loan] (:payments t))
        (run-sheet 4))))

(comment
  (let [t (price 0.1 3 1000)]
   (->> (balance-sheet [:pnl/revenue :asset/loan] (:interest t)
                       [:asset/cash :asset/loan] (:payments t))
        (run-sheet 4))))
