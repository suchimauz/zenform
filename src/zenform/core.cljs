(ns zenform.core
  (:require
   [cljs.pprint :as pprint]
   [re-frame.core :as rf]
   [zenform.model :as model]
   [zenform.inputs.core :as inputs]
   [zenform.inputs.codemirror :as cm]))

(def text-input #'inputs/text-input)
(def invalid-feedback #'inputs/invalid-feedback)
(def select #'inputs/select)
(def toggle #'inputs/toggle)
(def radio  #'inputs/radio)
(def codemirror  #'cm/input)

(defn form-value [form-path]
  (let [v (rf/subscribe [:zf/get-value form-path])]
    (fn []
      [:pre (with-out-str (pprint/pprint @v))])))

(defn node [form-path path]
  (let [v (rf/subscribe [:zf/node form-path path])]
    (fn []
      [:pre (with-out-str (pprint/pprint @v))])))

(defn pprint [v]
  [:pre (with-out-str (pprint/pprint v))])
