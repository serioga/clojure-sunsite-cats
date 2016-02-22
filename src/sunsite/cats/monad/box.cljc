(ns sunsite.cats.monad.box
  (:require [cats.protocols :as p]
            [cats.util :as util]
            [cats.context :as ctx]))

(defn exception?
  "Return true if `v` is an instance of
  the Throwable or js/Error type."
  [e]
  (instance? #?(:clj Exception :cljs js/Error) e))

;(defn- str-info [info]
;  (if (empty? info) "" (str " " (apply pr-str info))))
(defn- str-payload [info]
  (if (empty? info) "" (str " [" (str info) "]")))

(defn- str-exception [^Exception e] (.getMessage e))

(defn- ^String info-payload [info]
  (apply str (interpose " " info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types and implementations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare context)

(deftype Full [value payload]
  p/Contextual
  (-get-context [_] context)

  p/Extract
  (-extract [_] value)

  p/Printable
  (-repr [_]
    (str "#<Full " (pr-str value) (str-payload payload) ">"))

  #?@(:cljs [cljs.core/IDeref
             (-deref [_] value)]
      :clj  [clojure.lang.IDeref
             (deref [_] value)])

  #?@(:clj
      [Object
       (equals [self other]
         (if (instance? Full other)
           (= value (.value ^Full other))
           false))]
      :cljs
      [cljs.core/IEquiv
       (-equiv [_ other]
               (if (instance? Full other)
                 (= value (.value other))
                 false))]))

(deftype Failure [exception payload]
  p/Contextual
  (-get-context [_] context)

  p/Extract
  (-extract [_] exception)

  p/Printable
  (-repr [_]
    (str "#<Failure " (str-exception exception) (str-payload payload) ">"))

  #?@(:cljs [cljs.core/IDeref
             (-deref [_] (throw exception))]
      :clj  [clojure.lang.IDeref
             (deref [_] (throw exception))])

  #?@(:clj
      [Object
       (equals [self other]
         (if (instance? Failure other)
           (= exception (.exception ^Failure other))
           false))]

      :cljs
      [cljs.core/IEquiv
       (-equiv [_ other]
               (if (instance? Failure other)
                 (= exception (.exception other))
                 false))]))

(alter-meta! #'->Full assoc :private true)
(alter-meta! #'->Failure assoc :private true)

(util/make-printable Full)
(util/make-printable Failure)

(defn full?
  "Return true if `v` is an instance of
  the Full type."
  [v]
  (instance? Full v))

(defn failure?
  "Return true if `v` is an instance of
  the Failure type."
  [v]
  (instance? Failure v))

(defn box?
  "Return true in case of `v` is instance
  of Box monad."
  [v]
  (if (satisfies? p/Contextual v)
    (identical? (p/-get-context v) context)
    false))

(defn full
  "A Full type constructor.

  It wraps any arbitrary value into
  full box type."
  [v & info]
  (Full. v (info-payload info)))

(defn failure
  "A failure type constructor."
  ([]
   (failure (Exception.)))
  ([err & info]
   (let [payload (info-payload info)]
     (cond
       (exception? err) (Failure. err payload)

       (failure? err) (let [orig-exc (.exception ^Failure err)
                            orig-payload (str (.payload ^Failure err))
                            message (str-exception orig-exc)
                            chain #(->> %
                                        (remove empty?)
                                        (interpose " <- ")
                                        (apply str))]
                        (Failure. (Exception. message orig-exc)
                                  (chain [payload orig-payload])))

       :else (Failure. (Exception. ^String (str err)) payload)))))

(defn failure-message [box]
  (if (failure? box) (str (str-exception (.exception ^Failure box))
                          (str-payload (.payload ^Failure box)))))

(defn extract
  "Return inner value from exception monad.

  This is a specialized version of `cats.core/extract`
  for Exception monad types that allows set up
  the default value.

  If a provided `mv` is an instance of Failure type
  it will re raise the inner exception. If you need
  extract value without raising it, use `cats.core/extract`
  function for it."
  ([mv]
   {:pre [(box? mv)]}
   (if (full? mv)
     (p/-extract mv)
     (throw (p/-extract mv))))
  ([mv default]
   {:pre [(box? mv)]}
   (if (full? mv)
     (p/-extract mv)
     default)))

(defn ^{:no-doc true}
exec-try-on
  [func & msgs]
  (try
    (let [result (func)]
      (cond
        (exception? result) (apply failure result msgs)
        (failure? result) (apply failure result msgs)
        (box? result) result
        :else (full result)))
    (catch #?(:clj  Exception
              :cljs js/Error) e (apply failure e msgs))))

(defn ^{:no-doc true}
exec-try-or-else
  [func defaultvalue]
  (let [result (exec-try-on func)]
    (if (failure? result)
      (full defaultvalue)
      result)))

(defn ^{:no-doc true}
exec-try-or-recover
  [func recoverfn]
  (let [result (exec-try-on func)]
    (ctx/with-context context
      (if (failure? result)
        (recoverfn (.exception ^Failure result))
        result))))

#?(:clj
   (defmacro try-on
     "Wraps a computation and return full of failure."
     ([expr & msgs]
      `(let [func# (fn [] ~expr)]
         (if (empty? [~@msgs])
           (exec-try-on func# '~expr)
           (exec-try-on func# ~@msgs))))))

#?(:clj
   (defmacro try-or-else
     [expr defaultvalue]
     `(let [func# (fn [] ~expr)]
        (exec-try-or-else func# ~defaultvalue))))

#?(:clj
   (defmacro try-or-recover
     [expr func]
     `(let [func# (fn [] ~expr)]
        (exec-try-or-recover func# ~func))))

(defn wrap
  "Wrap a function in a try monad.

  Is a high order function that accept a function
  as parameter and returns an other that returns
  full or failure depending of result of the
  first function."
  [func]
  (let [metadata (meta func)]
    (-> (fn [& args] (try-on (apply func args)))
        (with-meta metadata))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:no-doc true}
context
  (reify
    p/Context
    (-get-level [_] ctx/+level-default+)

    p/Functor
    (-fmap [_ f s]
      (if (full? s)
        (try-on (f (p/-extract s)))
        s))

    p/Applicative
    (-pure [_ v]
      (full v))

    (-fapply [m af av]
      (if (full? af)
        (p/-fmap m (p/-extract af) av)
        af))

    p/Monad
    (-mreturn [_ v]
      (full v))

    (-mbind [_ s f]
      (if (full? s)
        (f (p/-extract s))
        s))

    p/Printable
    (-repr [_]
      "#<Box>")))

(util/make-printable (type context))
