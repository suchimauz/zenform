(ns zenform.model
  (:require [clojure.walk   :as walk]
            [clojure.string :as str]

            [re-frame.db        :as db]
            [re-frame.core      :as rf]

            [zenform.validators :as validators]))

(def ^:private form-change-atom
  "Stores the `id` of current handler and checking function
   
   This atom is needed for save independence of zenform from main app"
  (atom {:fx-id    nil
         :check-fn nil}))

(defn reg-form-change-fx
  "Registers fx that will be called when form value changes.
   
   There can be only one fx at a time."
  [id handler]
  (swap! form-change-atom assoc :fx-id id)
  (rf/reg-fx id handler))

(defn reg-checking-fn
  "Add function that will check needs to prevent data loss"
  [func]
  (swap! form-change-atom assoc :check-fn func))

(rf/reg-fx
 ::value-changed
 (fn [form-path]
   (when (or (not (:check-fn @form-change-atom))
             ((:check-fn @form-change-atom) form-path))
     (swap! db/app-db update-in form-path
            merge {:changed? true
                   :saved?   false}))))

(rf/reg-event-fx
 ::value-changed
 (fn [_ [_ form-path]]
   (cond-> {::value-changed form-path}
     (:fx-id @form-change-atom)
     (assoc (:fx-id @form-change-atom) form-path))))

(defn *form [{:keys [type default] :as sch} path val]
  (let [v (cond
            (some? val)   val
            (fn? default) (default)
            :else         default)]
    (cond
      (= :form type)
      (assoc (dissoc sch :fields) :value
             (reduce (fn [acc [k *sch]]
                       (let [v (get val k)]
                         (assoc acc k (*form *sch (conj path k) v))))
                     {} (:fields sch)))
      (= :collection type)
      (assoc sch :value
             (into {}
                   (map-indexed
                    (fn [i *val]
                      [i (*form
                          (dissoc (:item sch) :value)
                          (conj path i)
                          *val)]))
                   v))

      (= type :boolean)
      (assoc sch :value (boolean v))

      type
      (assoc sch :value v)

      :else v)))

(defn form
  "create form model from schema and defaults"
  [schema & [value]]
  (*form schema [] (or value {})))

(defn get-node-path  [pth]
  (reduce (fn [acc x] (conj acc :value x)) [] pth))

(defn get-value-path  [pth]
  (conj (get-node-path pth) :value))

(defn *get-value [form]
  (cond
    (and (:value form) (= :collection (:type form)))
    (let [res (->> (mapv *get-value (mapv second (sort-by first (:value form))))
                   (filterv #(not (nil? %))))]
      (when-not (empty? res) res))


    (and  (map? form) (map? (:value form)) (= :form (:type form)))
    (let [res (reduce (fn [acc [k node]]
                        (let [v (*get-value node)]
                          (if-not (nil? v)
                            (assoc acc k v)
                            acc))) {} (:value form))]
      (when-not (empty? res) res))


    :else (:value form)))


(defn get-value
  "Get value for specific path; if path not passed returns form value."
  ([db form-path path]
   (-> db (get-in form-path) (get-value path)))
  ([form path]
   (*get-value (get-in form (get-node-path path))))
  ([form]
   (*get-value form)))

(defn get-value!
  "Get value for specific path; if path not passed returns form value.
   
   Side-effect: gets form from the `app-db` in turn bypassing the normal flow of data."
  ([form-path path]
   (get-value (get-in @db/app-db form-path) path))
  ([form-path]
   (get-value (get-in @db/app-db form-path))))

(defn get-form-node
  ([form-data path]
   (get-in form-data (get-node-path path)))
  ([db form-path path]
   (-> db
       (get-in form-path)
       (get-in (get-node-path path)))))

(defn validate-node [node value & [path]]
  (reduce (fn [errs [k cfg]]
            (if-let [msg (let [arrity (some->> k
                                               (get-method validators/validate)
                                               validators/get-arrities
                                               (apply max)
                                               (#(- % 1)))]
                           (apply validators/validate
                                  (merge {:type k, :node node} cfg)
                                  (if arrity
                                    (take arrity [value path])
                                    [value path])))]
              (assoc errs k msg)
              errs))
          nil (:validators node)))


(defn fire-on-change [form-path form & [path]]
  (when-let [node (get-in form (or path []))]
    (when-let [change (:on-change node)]
      (let [path' (vec (remove #(= :value %) path))]
        (doall
         (for [[k args] change]
           (rf/dispatch [k (:value node) form-path path' args])))))
    (when (map? node)
      (reduce-kv (fn [path k _]
                   (fire-on-change form-path form (conj path k))
                   path)
                 (when (or (= :collection (:type node))
                           (= :form (:type node)))
                   (conj (or path []) :value))
                 (when (or (= :collection (:type node))
                           (= :form (:type node)))
                   (:value node))))))

(rf/reg-event-fx
 :zf/fire-on-change
 (fn [{db :db} [_ form-path]]
   (fire-on-change form-path (get-in db form-path))
   {}))

(defn *set-value [form _ path value & [type]]
  (let [value' (if (and (string? value) (str/blank? value)) nil value)
        path'  (if (= type :collection)
                 (get-node-path path)
                 (get-value-path path))]
    (assoc-in form path' value')))

(defn *on-value-set [node form-path path]
  (let [v    (*get-value node)
        errs (validate-node node v path)]
    (doall
     (for [[k & args] (:on-change node)]
       (rf/dispatch (apply vector k v form-path path args))))
    (cond-> (dissoc node :errors)
      errs (assoc :errors errs))))

(defn *on-value-set-loop [form form-path path]
  (loop [form form
         path path]
    (if (nil? path)
      (*on-value-set form form-path path)
      (recur (update-in form (get-node-path path) #(*on-value-set % form-path path))
             (butlast path)))))

(defn set-value
  "Put value for specific path; run validations."
  [form form-path path value & [type]]
  (let [value' (if (and (string? value)
                        (str/blank? value))
                 nil
                 value)
        path' (if (= type :collection)
                (get-node-path path)
                (get-value-path path))
        form' (assoc-in form path' value')]
    (-> form'
        (*set-value form-path path value' type)
        (*on-value-set-loop form-path path))))

(defn set-value!
  "Careful!"
  [form-path path value]
  (swap! db/app-db update-in form-path (fn [form]
                                         (set-value form form-path path value))))


(defn raw-value
  "Return raw form value"
  [v]
  (walk/prewalk
   (fn [x]
     (if (and (map? x) (:value x))
       (:value x)
       x)) v))

(defn indexed-value
  "Return indexed value"
  ([v]
   (walk/prewalk
    (fn [x]
      (if (and (map? x) (contains? x :value))
        (:value x)
        x)) v))
  ([v path]
   (-> (indexed-value v)
       (get-in path))))

;; this fn will fuck your brain
;; it evals all validators and collect errors
;; at the same time collect value
;; intended to be used at submit

(declare eval-errors)

(defn aggregate-errors
  "For nodes with type `:form` and `:collection`
   reduce by fields of items and collect all child errors."
  [form-value {node-value :value} node-index]
  (reduce-kv
   (fn [acc idx child-node]
     (let [node-path (conj node-index idx)
           errors (eval-errors form-value child-node node-path)]
       (merge acc errors)))
   {}
   node-value))

(defn eval-errors
  "Get all child errors if need, then validate node itself
   after that merge in to one big error."
  [form-value {node-type :type :as node} node-index]
  (let [child-errors (when (#{:collection :form} node-type)
                       (aggregate-errors form-value node node-index))
        node-errors (validate-node node (get-in form-value node-index))]
    (cond-> child-errors
      node-errors (assoc  node-index node-errors))))

(declare **eval-form)
(defn eval-form-node [{node-value :value}]
  (reduce-kv
   (fn [acc field child-node]
     (let [evaled-node (**eval-form child-node)]
       (assoc acc field (:value evaled-node))))
   {}
   node-value))

(defn eval-collection-node [{node-value :value}]
  (reduce-kv
   (fn [acc _ child-node]
     (let [evaled-node (**eval-form child-node)]
       (conj acc (:value evaled-node))))
   []
   node-value))

(defn eval-node [{node-type :type :as node}]
  (case node-type
    :form        (eval-form-node node)
    :collection  (eval-collection-node node)
    (:value node)))

(defn inject-errors [errors form]
  (reduce-kv
   (fn [acc path errs]
     (let [node-path (get-node-path path)]
       (assoc-in acc (conj node-path :errors) errs)))
   form
   errors ))

(defn **eval-form
  "Collect form value, then validate whole form and collect errors"
  [form]
  (let [value   (eval-node form)
        errors  (eval-errors value form [])
        form    (inject-errors errors form)]
    (cond-> {:value value
             :form form}
      errors (assoc :errors errors))))

(defn- *eval-form [{tp :type v :value :as node} & [pth]]
  (if (or (= tp :collection) (= tp :form))
    (let [{v :value :as res}
          (reduce (fn [res [idx n]]
                    (let [pth (conj (or pth []) idx)
                          {v :value err :errors files :files ch-node :form} (*eval-form n pth)]
                      (cond-> res
                        :always
                        (assoc-in [:form :value idx] ch-node)

                        (seq err)
                        (update :errors (fn [es]
                                              (reduce (fn [es [err-k err-v]]
                                                        (assoc es (into [idx] err-k) err-v))
                                                      es err)))

                        (seq files)
                        (update :files concat files)

                        (and (not (nil? v)) (= tp :collection))
                        (update :value conj v)

                        (and (not (nil? v)) (= tp :form))
                        (assoc-in [:value idx] v))))
                  {:value  (if (= tp :form)
                             {} [])
                   :errors {}
                   :form   node} v)
          errs (validate-node node v pth)]
      (cond-> res
        :always
        (update :value #(when-not (empty? %) %))

        errs
        (-> (assoc-in [:errors []] errs)
            (assoc-in [:form :errors] errs))))

    (let [errs  (validate-node node v pth)
          files (when (sequential? v)
                  (for [{:keys [file]} v
                        :when file]
                    file))
          node (cond-> node
                 (map? node)
                 (assoc :touched true)

                 errs
                 (assoc :errors errs))]
      (cond-> {:value (cond-> v
                        (map? v)
                        (dissoc :file))
               :form  node}
        errs
        (assoc :errors {[] errs})

        files
        (assoc :files files)))))

(defn eval-form [form]
  (*eval-form form))

(rf/reg-event-db
 :zf/init
 (fn [db [_ form-path schema value]]
   (assoc-in db form-path (form schema value))))

(rf/reg-event-fx
 :zf/set-value
 (fn [{db :db} [_ form-path path v & [{:keys [success init?]}]]]
   (cond-> {:db (update-in db form-path (fn [form]
                                          (set-value form form-path path v)))
            :fx []}
     
     (not init?)
     (update :fx conj [:dispatch [::value-changed form-path]])

     success
     (update :fx conj [:dispatch [(:event success) (:params success)]]))))

(rf/reg-event-fx
 :zf/update-value
 (fn [{db :db} [_ form-path path f & [{:keys [success]}]]]
   (cond-> {:db (update-in db form-path
                           (fn [form]
                             (let [v  (get-value form path)
                                   v' (f v)]
                               (set-value form form-path path v'))))
            :fx [[:dispatch [::value-changed form-path]]]}
     success
     (update :fx conj [:dispatch [(:event success) (:params success)]]))))

(rf/reg-event-fx
 :zf/clear-value
 (fn [{db :db} [_ form-path path]]
   (let [path* (get-value-path path)]
     {:db (assoc-in db (into form-path path*) nil)
      :dispatch [::value-changed form-path]})))

(rf/reg-event-fx
 :zf/set-values
 (fn [{db :db} [_ form-path path vs]]
   {:db  (update-in db form-path
                    (fn [form]
                      (reduce-kv
                       (fn [acc k v]
                         (set-value acc form-path
                                    (into path (cond-> k (keyword? k) (vector)))
                                    v))
                       form
                       vs)))
    :dispatch [::value-changed form-path]}))

(rf/reg-sub
 :zf/collection-indexes
 (fn [db [_ form-path path]]
   (-> db
       (get-in form-path)
       (get-in (get-node-path path))
       :value keys)))

(rf/reg-sub
 :zf/node
 (fn [db [_ form-path path]]
   (get-form-node db form-path path)))

(rf/reg-sub
 :zf/form
 (fn [db [_ form-path]]
   (get-in db form-path)))

(rf/reg-sub
 :zf/get-value
 (fn [db [_ form-path path]]
   (let [form (get-in db form-path)]
     (if path
       (get-value form path)
       (get-value form)))))

(rf/reg-sub
 :zf/indexed-value
 (fn [db [_ form-path path]]
   (let [form (get-in db form-path)]
     (if path
       (indexed-value form path)
       (indexed-value form)))))

(defn add-collection-item [form form-path path v]
  (let [node (get-in form (get-node-path path))]
    (if (= :collection (:type node))
      (let [coll (:value node)
            idx (if (empty? coll)
                  0
                  (inc (first (apply max-key key coll))))
            sch (:item node)
            v (*form sch [] (or v {}))]
        (-> (*set-value form form-path (conj path idx) v (:type node))
            (*on-value-set-loop form-path path)))
      form)))

(defn remove-collection-item [form path idx & [form-path]]
  (let [node-path (get-node-path path)]

    (if (= :collection (get-in form (conj node-path :type)))
      (-> (update-in form (conj node-path :value) dissoc idx)
          (*on-value-set-loop form-path path))
      form)))

(defn set-collection [form path v]
  (let [node (get-in form (get-node-path path))]
    (if (= :collection (:type node))
      (set-value form path (*form node [] (or v {})) (:type node))
      form)))

(defn set-collection-item
  [form-data form-path path v]
  (let [node   (get-in form-data (get-node-path (butlast path)))
        schema (:item node)
        value  (*form schema [] (or v {}))]
    (-> (*set-value form-data form-path path value :collection)
        (*on-value-set-loop form-path (butlast path)))))

(rf/reg-event-fx
 :zf/add-collection-item
 (fn [{db :db} [_ form-path path v]]
   {:db (update-in db form-path (fn [form] (add-collection-item form form-path path v)))
    :dispatch [::value-changed form-path]}))

(rf/reg-event-fx
 :zf/remove-collection-item
 (fn [{db :db} [_ form-path path idx]]
   {:db (update-in db form-path (fn [form] (remove-collection-item form path idx form-path)))
    :dispatch [::value-changed form-path]}))

(rf/reg-event-fx
 :zf/set-collection
 (fn [{db :db} [_ form-path path v]]
   {:db (update-in db form-path (fn [form] (set-collection form path v)))
    :dispatch [::value-changed form-path]}))

(rf/reg-event-fx
 :zf/set-collection-item
 (fn [{db :db} [_ form-path path v]]
   {:db (update-in db form-path (fn [form-data] (set-collection-item form-data form-path path v)))
    :dispatch [::value-changed form-path]}))

(rf/reg-sub
 :zf/collection
 (fn [[_ form-path path] _]
   (rf/subscribe [:zf/node form-path path]))
 (fn [node _]
   (:value node)))

(defn get-full-path
  [form-path path]
  (into form-path (get-node-path path)))

(rf/reg-event-fx
 :zf/update-node-schema
 (fn [{db :db} [_ form-path path value]]
   {:db (update-in db
                   (get-full-path form-path path)
                   #(merge % value))}))

(rf/reg-event-fx
 :zf/update-nodes-in-schema
 (fn [{db :db} [_ form-path payload]]
   {:db (reduce
         (fn [acc [path value]]
           (update-in acc (get-full-path form-path path) #(merge % value)))
         db
         payload)}))

(rf/reg-event-fx
 :zf/remove-validators
 (fn [{db :db} [_ form-path path]]
   {:db (update-in db
                   (get-full-path form-path path)
                   #(dissoc % :validators))}))


(def before-transition
  "Calculates possible data loss and assocs it in coeffects."
  (rf/->interceptor
   :id     :before-transition
   :before (fn [ctx]
             (let [{:form/keys [prevent-data-loss?]}
                   (last (get-in ctx [:coeffects :db :route-map/current-route :parents]))
                   
                   forms (get-in ctx [:coeffects :db :form])]
               (cond-> ctx
                 (some (fn [[_ {:keys [changed? saved? data-loss dialog?]}]]
                         (and changed? (not saved?) (not (#{:ignored} data-loss))
                              (or prevent-data-loss?
                                  dialog?)))
                       forms)
                 (assoc-in [:coeffects :zf/possible-data-loss?] true))))))

(rf/reg-event-fx
 ::ignore-data-loss
 (fn [{db :db} [_ {:keys [success]}]]
   (cond-> {:db (update db :form (partial reduce-kv
                                          (fn [acc k v]
                                            (assoc acc k 
                                                   (assoc v :data-loss :ignored)))
                                          {}))}
     success
     (assoc :dispatch [(:event success) (:params success)]))))

(rf/reg-event-fx
 :zf/data-saved
 (fn [{db :db} [_ {:keys [success form-pathes]}]]
   (cond-> {:db (reduce (fn [db form-path]
                          (update-in db form-path assoc :saved? true))
                        db
                        form-pathes)}
     success
     (assoc :dispatch [(:event success) (:params success)]))))
