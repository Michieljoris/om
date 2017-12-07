(ns om.util
  (:refer-clojure :exclude [ident?]))

(defn force-children [x]
  (cond->> x
    (seq? x) (into [] (map force-children))))
;;(force-children (list [:fox (list :rab)]:foo (list :bar (list :bax) {:a :b})))
;; => [[:fox (:rab)] :foo [:bar [:bax] {:a :b}]]
;;(seq? (list :foo))
;; => true

(defn union?
  #?(:cljs {:tag boolean})
  [expr]
  (let [expr (cond-> expr (seq? expr) first)]
    (and (map? expr)
      (map? (-> expr first second)))))
;;(union? {:a {:b :c}})
;; => true
;(union? (list {:a {:b :c}}))
;; => true

(defn join? [x]
  #?(:cljs {:tag boolean})
  (let [x (if (seq? x) (first x) x)]
    (map? x)))
;;(join? {:a :b}) ;; => true
;;(join? '({:a :b})) ;; => true

(defn ident?
  "Returns true if x is an ident."
  #?(:cljs {:tag boolean})
  [x]
  (and (vector? x)
    (== 2 (count x))
    (keyword? (nth x 0))))
;; (ident? [1 :b]) ;; => false
;; (ident? [:a :b]) ;; => true

(defn join-entry [expr]
  (if (seq? expr)
    (ffirst expr)
    (first expr)))
;;(join-entry {:a [:b :c]})
;; => [:a [:b :c]]

(defn join-key [expr]
  (cond
    (map? expr) (ffirst expr)
    (seq? expr) (join-key (first expr))
    :else       expr))
(join-key {:a [:b :c]})
;; => :a

(defn join-value [join]
  (second (join-entry join)))

(defn unique-ident?
  #?(:cljs {:tag boolean})
  [x]
  (and (ident? x) (= '_ (second x))))

(defn recursion?
  #?(:cljs {:tag boolean})
  [x]
  (or #?(:clj (= '... x)
         :cljs (symbol-identical? '... x))
      (number? x)))

(defn mutation?
  #?(:cljs {:tag boolean})
  [expr]
  (let [expr (cond-> expr (seq? expr) first)]
    (symbol? expr)))

(defn mutation-key [expr]
  {:pre [(symbol? (first expr))]}
  (first expr))
