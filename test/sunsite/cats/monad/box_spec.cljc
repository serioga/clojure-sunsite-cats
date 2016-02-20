(ns sunsite.cats.monad.box-spec
  #?@(:cljs
      [(:require [cljs.test :as t]
         [clojure.test.check]
         [clojure.test.check.generators :as gen]
         [clojure.test.check.properties :as prop :include-macros true]
         [cats.labs.test :as lt]
         [cats.builtin :as b]
         [cats.protocols :as p]
         [sunsite.cats.monad.box :as box :include-macros true]
         [cats.monad.either :as either]
         [cats.context :as ctx :include-macros true]
         [cats.core :as m :include-macros true])
       (:require-macros [clojure.test.check.clojure-test :refer (defspec)])])
  #?(:clj
     (:require [clojure.test :as t]
               [clojure.test.check.clojure-test :refer [defspec]]
               [clojure.test.check :as tc]
               [clojure.test.check.generators :as gen]
               [clojure.test.check.properties :as prop]
               [cats.labs.test :as lt]
               [cats.builtin :as b]
               [cats.protocols :as p]
               [sunsite.cats.monad.box :as box]
               [cats.monad.either :as either]
               [cats.context :as ctx]
               [cats.core :as m])))

(t/deftest basic-operations-test
  (let [e #?(:clj  (Exception. "test")
             :cljs (js/Error.  "test"))]
    (t/is (= 1 (m/extract (box/try-on 1))))
    (t/is (= e (m/extract (box/try-on (throw e)))))
    (t/is (= e (m/extract (box/try-on e))))))

#?(:clj
   (t/deftest ideref-test
     (t/is (= 1 @(box/full 1)))
     (t/is (thrown? Exception @(box/failure {:message "foobar"})))))

(t/deftest predicates-test
  (let [m1 (box/full 1)
        m2 (box/failure {})]
    (t/is (box/full? m1))
    (t/is (box/failure? m2))))


(t/deftest wrapping-test
  (let [func #?(:clj (fn [x] (+ x nil))
                :cljs (fn [x] (throw (js/Error. "test"))))
        func (box/wrap func)]
    (t/is (box/failure? (func 3)))))

#?(:clj
   (t/deftest try-or-else-test
     (let [m1 (box/try-or-else (+ 1 nil) 40)]
       (t/is (box/full? m1))
       (t/is (= 40 (box/extract m1))))))

#?(:clj
   (t/deftest try-or-recover-test
     (let [m1 (box/try-or-recover (+ 1 nil) (fn [e] (m/return 60)))]
       (t/is (box/full? m1))
       (t/is (= 60 (box/extract m1))))

     (let [m1 (box/try-or-recover
                (+ 1 nil)
                (fn [e] (either/right 60)))]
       (t/is (either/right? m1))
       (t/is (= 60 (m/extract m1))))))

#?(:cljs
   (t/deftest try-or-recover-test
     (let [e  (js/Error. "test")
           m1 (box/try-or-recover e (fn [e] (m/return 60)))]
       (t/is (box/full? m1))
       (t/is (= 60 (box/extract m1))))

     (let [e  (js/Error. "test")
           m1 (box/try-or-recover e (fn [e] (either/right 60)))]
       (t/is (either/right? m1))
       (t/is (= 60 (m/extract m1))))))

#?(:clj
   (t/deftest try-on-macro-test
     (let [m1 (box/try-on (+ 1 nil))]
       (t/is (instance? NullPointerException (m/extract m1))))))

#?(:cljs
   (t/deftest try-on-macro-test
     (let [m1 (box/try-on (js/Error. "foo"))]
       (t/is (instance? js/Error (m/extract m1))))))

#?(:clj
   (t/deftest functor-test
     (let [m1 (box/try-on 1)
           m2 (box/try-on nil)]
       (t/is (instance? NullPointerException
               (m/extract (m/fmap inc m2))))
       (t/is (= (box/full 2) (m/fmap inc m1))))))

;; Generators

(defn successes-of [g]
  (gen/fmap box/full g))

(def full-gen
  (successes-of gen/any))

(def failure-gen
  (gen/return (box/failure {})))

(def box-gen
  (gen/one-of [full-gen failure-gen]))

(def vectors-gen
  (gen/vector gen/any))

;; Functor

(defspec box-first-functor-law 10
  (lt/first-functor-law
    {:gen box-gen}))

(defspec box-second-functor-law 10
  (lt/second-functor-law
    {:gen box-gen
     :f   str
     :g   count}))

;; Applicative

(defspec box-applicative-identity 10
  (lt/applicative-identity-law
    {:ctx box/context
     :gen box-gen}))

(defspec box-applicative-homomorphism 10
  (lt/applicative-homomorphism
    {:ctx box/context
     :gen gen/any
     :f   (constantly false)}))

(defspec box-applicative-interchange 10
  (lt/applicative-interchange
    {:ctx  box/context
     :gen  gen/int
     :appf (box/full inc)}))

(defspec box-applicative-composition 10
  (lt/applicative-composition
    {:ctx  box/context
     :gen  gen/int
     :appf (box/full inc)
     :appg (box/full dec)}))

;; Monad

(defspec box-first-monad-law 10
  (lt/first-monad-law
    {:ctx box/context
     :mf  #(if % (box/full %) (box/failure {}))}))

(defspec box-second-monad-law 10
  (lt/second-monad-law {:ctx box/context}))

(defspec box-third-monad-law 10
  (lt/third-monad-law
    {:ctx box/context
     :f   (comp box/full str)
     :g   (comp box/full count)}))
