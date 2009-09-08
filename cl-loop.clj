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




(def assign  { :name "set"     , :args 3, :bind (fn [[_ var val]] `(~var ~val))})
(def if      { :name "if"      , :args 4, :push if-push})
(def collect { :name "collect" , :args 2, :push collect-push})
(def conc    { :name "concat"  , :args 2, :push concat-push})
(def seq-iteration
     {:name  "for"
      :args  3
      :vars  (fn [x] (gensym "seq-iter__"))
      :bind  gen-seq-iter-bind
      :test  gen-seq-iter-test
      :init  gen-seq-iter-init
      :recur gen-seq-iter-recur})

(defonce *handlers* (atom {}))

(defn register [& handlers]
  (swap! *handlers*
         (fn [val]
           (reduce (fn [x y] (assoc x (:name y) y))
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
               (#{"for" "set" "collect" "concat"} (name (first forms))))
          (let [args (num-args-for-form (name (first forms)))]
            (recur (drop args forms) (conj results (into [] (take args forms)))))
          (coll? (first forms))
          (recur (rest forms) (conj results (first forms)))
          true
          (throw (IllegalArgumentException.
                  (str "Can't parse " (into [] forms) " within " init-forms))))))


(defmacro cl-loop [& forms]
  (let [normalized-forms (normalize-forms forms)

        seq-iter-forms   (filter seq-iter-form? normalized-forms)
        seq-iter-gensyms (map (fn [x] (gensym "seq-iter__")) seq-iter-forms)
        seq-iter-inits   (map gen-seq-iter-init  seq-iter-forms seq-iter-gensyms)
        seq-iter-tests   (map gen-seq-iter-test  seq-iter-forms seq-iter-gensyms)
        seq-iter-binds   (map gen-seq-iter-bind  seq-iter-forms seq-iter-gensyms)
        seq-iter-recurs  (map gen-seq-iter-recur seq-iter-forms seq-iter-gensyms)

        set-forms        (filter (fn [x] (= "set" (name (first x))))
                                 normalized-forms)
        set-binds        (map (fn [[_ var val]] `(~var ~val)) set-forms)

        if-forms         (filter (fn [x] (= "if" (name (first x))))
                                 normalized-forms)
        if-pushes        (map if-push if-forms)

        collect-forms    (filter collect-form? normalized-forms)
        collect-pushes   (map collect-push collect-forms)

        concat-forms     (filter concat-form? normalized-forms)
        concat-pushes    (map concat-push concat-forms)

        inits            (apply concat (concat seq-iter-inits))
        binds            (apply concat (concat seq-iter-binds set-binds))
        pushes           (concat collect-pushes concat-pushes if-pushes)
        recurs           (concat seq-iter-recurs)]
    `(binding [*default-collector* []]
       (loop [~@inits]
         (if (and ~@seq-iter-tests)
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