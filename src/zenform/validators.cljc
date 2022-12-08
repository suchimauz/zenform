(ns zenform.validators
  (:require [clojure.string :as str]))

(declare validate*)

(defmulti validate
  {:arglists '([cfg value & [path]])}
  #'validate*)

(defn get-arrities
  "Returns hash-set of numbers of arrities of a function.
   
   For variadic arities returns (+ 1 number-of-fixed-arguments)"
  [fn]
  #?(:clj  (->> fn class
                .getDeclaredMethods
                (map #(-> %
                          .getParameterTypes
                          alength))
                set)
     :cljs (if (->> fn js-keys count
                    ((partial = 0)))
             (hash-set (.-length fn))
             (cond-> (->> fn js-keys
                          (map #(js/parseInt (re-find #"\d*$" %)))
                          (remove js/isNaN)
                          set)
               (->> fn js-keys
                    (some (partial re-find #"variadic")))
               ((partial cons (+ 1 (.-cljs$lang$maxFixedArity fn))))

               :always
               set))))

#_(defn variadic-multimethod
  "Allows for creation of variadic multimethods.
   
   Sideeffecty: defs method for multi called `:regress` which is used to reduce number of params.
   
   Never used."
  [multi method params]
  (when (nil? (get-method multi :regress))
    (defmethod multi :regress
      [method & params]
      (apply multi method (butlast params))))
  (let [arrities (some->> method
                          (get-method multi)
                          get-arrities)]
    (cond
      (nil? (get-method multi method))
      (if (get-method multi :default)
        :default
        method)

      (arrities (count (conj params method)))
      method

      (nil? params)
      (throw (#?(:clj  Exception.
                 :cljs js/Error.) "Can't find method with valid arity"))

      :else
      :regress)))

(defn validate*
  [cfg & _]
  (:type cfg))

(defmethod validate
  :min-length
  [{limit :value msg :message} v]
  (when (string? v)
    (when (< (count v) limit)
      (or msg (str "Shouldn't be shorter then " limit)))))

(defmethod validate
  :min-items
  [{limit :value msg :message} v]
  (when (coll? (or v []))
    (when (< (count v) limit)
      (or msg (str "Shouldn't be shorter then " limit)))))

(defmethod validate
  :number
  [{msg :message} v]
  (when-not
      (and (not (if (string? v)
                  (str/blank? v)
                  (empty? v)))
           (or (>= v 0) (<= v 0)))
    (or msg "Should be a number")))

(defmethod validate
  :required
  [{msg :message} v]
  (when (or (nil? v)
            (and (string? v) (str/blank? v))
            (and (map? v) (empty? v))
            (and (sequential? v) (empty? v)))
    (or msg "Should not be blank")))

(defmethod validate :pseudo
  [& _]
  nil)

(def email-regex #".+?\@.+?\..+")

(defmethod validate
  :email
  [{msg :message} v]
  (when v
    (when-not (re-matches email-regex v)
      (or msg "Should be valid email"))))

(defmethod validate
  :pattern
  [{rx :regex msg :message} v]
  (when v
    (when-not (re-matches rx v)
      (or msg (str "Should match " rx)))))