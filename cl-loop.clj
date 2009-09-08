(use 'clojure.contrib.pprint)
(use 'clojure.contrib.def)

(def *default-collector*)

(defn seq-iter-form? [x]
  (= "for" (name (first x))))
(defn gen-seq-iter-init [form a-gensym]
  (let [[prefix var list-form] form]
   `(~a-gensym (seq ~list-form))))
(defn gen-seq-iter-test [form a-gensym]
  (let [[prefix var list-form] form]
    `(seq ~a-gensym)))
(defn gen-seq-iter-bind [form a-gensym]
  (let [[prefix var list-form] form]
    `(~var (first ~a-gensym))))
(defn gen-seq-iter-recur [form a-gensym]
  (let [[prefix var list-form] form]
    `(rest ~a-gensym)))

(defn collect-form? [x]
  (= "collect" (name (first x))))
(defn collect-push [[_ var] & rest]
  `(set! *default-collector* (conj *default-collector* ~var)))

(defn concat-form? [x]
  (= "concat" (name (first x))))
(defn concat-push [[_ var] & rest]
  `(set! *default-collector* (into [] (concat *default-collector* ~var))))

(defn assign-bind [[_ var val] & rest]
  `(~var ~val))

(declare process-push)

(defn if-push [form & rest]
  (let [[_ test form-true & form-falses] form]
   `(if ~test
      ~(process-push form-true)
      ~@(map process-push form-falses))))

(defn push-fn [form]
  ({"collect" collect-push, "concat" concat-push "if" if-push}
   (name (first form))))

(defn process-push [form]
  ((push-fn form) form))


(defstruct verb :word :args :extra-args :bind :test :init :recur)

(defn make-verb [& args]
  (with-meta (apply struct-map verb args)
             {:type ::verb}))

(defmethod print-method ::verb [obj writer]
  (cl-format writer "#<verb ~a>" (:word obj)))

(def assign  (make-verb :word "set"     , :args 3, :bind assign-bind))
(def if      (make-verb :word "if"      , :args 4, :push if-push))
(def collect (make-verb :word "collect" , :args 2, :push collect-push))
(def conc    (make-verb :word "concat"  , :args 2, :push concat-push))
(def seq-iteration
     (make-verb
      :word  "for"
      :args  3
      :extra-args  (fn [x] (list (gensym "seq-iter__")))
      :bind  gen-seq-iter-bind
      :test  gen-seq-iter-test
      :init  gen-seq-iter-init
      :recur gen-seq-iter-recur})

(defonce *handlers* (atom {}))

(defn register [& handlers]
  (swap! *handlers*
         (fn [val]
           (reduce (fn [x y] (assoc x (:word y) y))
                   val
                   handlers))))

(register assign if collect conc seq-iteration)






(defn num-args-for-form [name]
  (:args (@*handlers* name)))

(defn normalize-forms [init-forms]
  (loop [forms init-forms results []]
    (cond (not (seq forms))
          results
          (and (symbol? (first forms))
               ((into #{} (keys @*handlers*)) (name (first forms))))
          (let [args (num-args-for-form (name (first forms)))]
            (recur (drop args forms) (conj results (into [] (take args forms)))))
          (coll? (first forms))
          (recur (rest forms) (conj results (first forms)))
          true
          (throw (IllegalArgumentException.
                  (str "Can't parse " (into [] forms) " within " init-forms))))))


(defnk cl-filter [pred coll :key-fn identity]
  (filter #(pred (key-fn %))
          coll))

(defn good-keys [handler]
  (let [available-keys (into #{} (for [[k v] handler :when v]
                                   k))]
    (clojure.set/intersection
     available-keys
     #{:bind :test :init :recur :push})))

(defn good-forms [forms handler]
  (cl-filter #(= % (:word handler))
             forms
             :key-fn (comp name first)))

(defn process-clauses [handler forms]
  (let [{:keys [word extra-args bind test init recur]} handler
        forms (good-forms forms handler)
        extra-args (if extra-args
                     (map extra-args forms)
                     (repeat (list nil)))]
    (into {} (for [good-key (good-keys handler)]
               (let [vals (loop [good-forms forms
                                 extra-args extra-args
                                 results []]
                            (if (seq good-forms)
                              (let [form (first good-forms)
                                    arg  (first extra-args)]
                                (recur (rest good-forms)
                                       (rest extra-args)
                                       (conj results
                                             (apply (good-key handler)
                                                    form
                                                    arg))))
                              results))]
                 [good-key vals])))))

#_(reduce (fn [x y] (merge-with concat x y))
          (for [h (vals @*handlers*)]
            (process-clauses h
                             '([for x [1 2 3]]
                                 [collect x]))))

(defmacro cl-loop [& forms]
  (let [normalized-forms (normalize-forms forms)

        stuff (reduce (fn [x y] (merge-with concat x y))
                      (for [h (vals @*handlers*)]
                        (process-clauses h
                                         normalized-forms)))

        inits            (reduce concat [] (:init stuff))
        binds            (reduce concat [] (:bind stuff))
        tests            (:test stuff)
        pushes           (:push stuff)
        recurs           (:recur stuff)]
    `(binding [*default-collector* []]
       (loop [~@inits]
         (if (and ~@tests)
           (let [~@binds]
             ~@pushes
             (recur ~@recurs))
           *default-collector*)))))



#_(cl-loop for letters [:a :b :c :d :e :f]
         for whole-nums (iterate inc 1)
         set mults-of-10 (* 10 whole-nums)
         (if (even? whole-nums)
           (if (even? whole-nums)
             [collect letters]
             [collect mults-of-10])
           (if true
             (if (odd? whole-nums)
               [collect mults-of-10]
               [collect letters]))))


(comment "

Need to add default # of arguments, so collect doesn't need
[]'s if 2 arg version? Hard to code-walk? (Can provide codewalker utils.)

do.. form which executes stuff in series.

finally.. and finally-return... forms.

Better error-handling during normalization and when translating forms.

when/unless... support

Make lazy? Lazyloop?

")