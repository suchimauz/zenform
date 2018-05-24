(ns zenform.node
  (:refer-clojure :exclude [coerce])
  (:require [zenform.validators :as val]
            [clojure.walk :as walk]
            [clojure.string :as s]
            #?(:clj [zenform.util :refer [with-catch]]
               :cljs [zenform.util :refer-macros [with-catch]])))

;;
;; Defaults
;;

(def node-defaults
  {:type nil
   :id nil
   :validators nil
   :errors nil
   :on-change nil})

(def form-defaults
  (merge
   node-defaults
   {:type :form
    :fields nil
    :defaults nil}))

(def coll-defaults
  (merge
   node-defaults
   {:type :coll
    :fields nil}))

(def field-defaults
  (merge
   node-defaults
   {:type :field
    :field-type nil
    :value nil
    :required? false
    :message-required "This value is required"
    :message-parse "Wrong field value"}))

;;
;; Helpers
;;

(defn head-tail [coll]
  [(first coll) (rest coll)])

(defn node? [node]
  (and
   (map? node)
   (get #{:form :coll :field} (:type node))))

(defn node-children
  [node]
  (condp = (:type node)
    :form (-> node :fields vals)
    :coll (-> node :fields)))

(defn iter-node [node]
  (tree-seq node? node-children node))

;;
;; Setting fields
;;

(defmulti set-fields :type)

(defmethod set-fields :form
  [form fields]
  (assoc form :fields
         (into {} (for [{id :id :as field} fields]
                    [id field]))))

(defmethod set-fields :coll
  [coll fields]
  (assoc coll :fields fields))

;;
;; Constructors
;;

(defn make-coll
  [id fields & [opt]]
  (-> coll-defaults
      (merge opt)
      (assoc :type :coll)
      (assoc :id id)
      (set-fields fields)))

(defn make-form
  [id fields & [opt]]
  (-> form-defaults
      (merge opt)
      (assoc :type :form)
      (assoc :id id)
      (set-fields fields)))

(defn make-field
  [field-type id & [opt]]
  (-> field-defaults
      (merge opt)
      (assoc :id id :field-type field-type)))

(def text-field (partial make-field :text))

(def integer-field (partial make-field :integer))

(def boolean-field (partial make-field :boolean))

;;
;; Values
;;

(defmulti get-value :type)

(defmethod get-value :form
  [{:keys [fields]}]
  (into {} (for [[id node] fields]
             [id (get-value node)])))

(defmethod get-value :coll
  [coll]
  (mapv get-value (:fields coll)))

(defmethod get-value :field
  [field]
  (:value field))

(defn clear-value
  [node]
  (assoc node :value nil))

(defmulti set-value :type)

(defmethod set-value :form
  [{:keys [fields] :as form} values]
  (reduce-kv
   (fn [form id field]
     (if (contains? values id)
       (let [value (get values id)]
         (update-in form [:fields id] set-value value))
       form))
   form
   fields))

(defmethod set-value :coll
  [{:keys [fields] :as coll} values]
  (let [values (take (count fields) values)
        pairs (map-indexed vector values)]
    (loop [coll coll
           pairs pairs]
      (if (empty? pairs)
        coll
        (let [[pair pairs] (head-tail pairs)
              [index value] pair
              coll (update-in coll [:fields index] set-value value)]
          (recur coll pairs))))))

(defmethod set-value :field
  [field value]
  (assoc field :value value))

;;
;; Errors
;;

(defn set-errors
  [node errors]
  (assoc node :errors errors))

(defn set-error
  [node error]
  (set-errors node [error]))

(defn clear-errors
  [node]
  (set-errors node nil))

(defmulti get-errors :type)

(defmethod get-errors :field
  [{:keys [errors]}]
  errors)

(defmethod get-errors :coll
  [{:keys [errors fields]}]
  {:errors errors
   :fields (mapv get-errors fields)})

(defmethod get-errors :form
  [{:keys [errors fields]}]
  {:errors errors
   :fields (into {} (for [[id field] fields]
                      [id (get-errors field)]))})

;;
;; Coercion
;;

(defmulti parse :field-type)

(defmulti unparse :field-type)

(defn parse-safe [field]
  (with-catch (parse field)))

(defmethod parse :text
  [{:keys [value]}]
  (cond
    (string? value) value
    :else (str value)))

#?(:clj
   (defn parseInt [x]
     (Integer/parseInt x)))

#?(:cljs
   (defn parseInt [x]
     (let [val (js/parseInt x)]
       (when-not (js/isNaN val)
         val))))

(defmethod parse :integer
  [{:keys [value] :as field}]
  (cond
    (int? value) value
    (string? value)
    (-> value s/trim parseInt)))

(defn empty-value?
  [x]
  (cond
    (nil? x) true
    (and (string? x) (s/blank? x)) true
    :else false))

(defn coerce
  [{:keys [value message-parse] :as field}]
  (if (empty-value? value)
    field
    (let [value (parse-safe field)]
      (if (some? value)
        (set-value field value)
        (set-error field message-parse)))))
;;
;; Validation
;;

(defmulti validate-required :type)

(defmethod validate-required :default
  [node] node)

(defmethod validate-required :field
  [{:keys [value required? message-required] :as node}]
  (if (and required? (empty-value? value))
    (set-error node message-required)
    node))

(defn apply-validator
  [{:keys [errors] :as node}
   {:keys [message] :as validator}]
  (if errors
    node
    (let [value (get-value node)]
      (if (val/validate-safe validator value)
        node
        (set-error node message)))))

(defn validate-validators
  [{:keys [validators] :as node}]
  (loop [node node
         validators validators]
    (if (empty? validators)
      node
      (let [[val vals] (head-tail validators)
            node (apply-validator node val)]
        (recur node vals)))))

(defn validate-node [node]
  (-> node validate-required validate-validators))

;; TODO validate required

(defn walker-validate [node]
  (if (node? node)
    (validate-node node)
    node))

(defn validate-all
  [node]
  (walk/postwalk walker-validate node))

;;
;; Paths and fields
;;

(defn get-field-path [path]
  (interleave (repeat :fields) path))

(defn get-field
  [node path]
  (get-in node (get-field-path path)))

(defn update-form
  [form path field-func & args]
  (let [field-path (get-field-path path)
        field (get-in form field-path)]
    (if field
      (apply update-in form field-path field-func args)
      form)))

;;
;; Events
;;

(defn dispatch-change
  [{:keys [on-change] :as node} & [params]]
  (when on-change
    (println ::on-change params)) ;; TODO dispatch
  node)

;;
;; Triggers
;;

(defmulti trigger-input :type)

(defmethod trigger-input :default
  [node] node)

(defmethod trigger-input :field
  [field value]
  (-> field
      (clear-errors)
      (set-value value)))

(defmulti trigger-value :type)

(defmethod trigger-value :default
  [node] node)

(defmethod trigger-value :field
  [field value]
  (-> field
      (trigger-input value)
      (coerce)
      (validate-node)))

(defn form-trigger-input
  [form path value]
  (update-form form path trigger-input value))

(defn form-trigger-value
  [form path value]
  (update-form form path trigger-value value))

;;
;; Bubbling
;;

(defn upward-paths
  [path]
  (loop [result []
         path path]
    (if (empty? path)
      result
      (let [result (conj result path)
            path (butlast path)]
        (recur result path)))))

(defn form-trigger-bubbling
  [form path value]
  (let [paths (upward-paths path)]
    (loop [form form
           paths paths]
      (if (empty? paths)
        (form-trigger-value form path value) ;; trigger the top-level node
        (let [[path paths] (head-tail paths)
              form (form-trigger-value form path value)]
          (recur form paths))))))
