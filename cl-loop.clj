(use 'clojure.contrib.pprint)
(use 'clojure.contrib.def)

(defn process-assign [form]
  (let [[_ var val] form]
    {:bind `(~var ~val)}))

(defn process-collect [form]
  (let [[_ var] form]
    {:push `(swap! ~'default-collector
                   (fn [collector#] (into [] (conj collector# ~var))))}))

(defn process-concatenation [form]
  (let [[_ var] form]
    {:push `(swap! ~'default-collector
                   (fn [collector#] (into [] (concat collector# ~var))))}))

(defn process-seq-iteration [form]
  (let [[prefix var list-form] form
        a-gensym (gensym "seq-iter__")]
    {:init  `(~a-gensym (seq ~list-form))
     :test  `(seq ~a-gensym)
     :bind  `(~var (first ~a-gensym))
     :recur `(rest ~a-gensym)}))

(defn process-sum [form]
  (let [[_ var] form]
    {:push `(swap! ~'default-collector
                   (fn [collector#] (+  (or collector# 0) ~var)))}))

(declare process-push)

(defn process-conditional [form]
  (throw (Exception. (cl-format nil "In cl-loop: Sorry, can't process ~%  ~S~%because conditionals are currently buggy." form)))
  (let [[_ test form-true & form-false] form]
    {:push `(if ~test
              ~(process-push form-true)
              ~@(map process-push form-false))}))

#_(defn push-fn [form]
  ({"collect" collect-push, "concat" concat-push "if" if-push}
   (name (first form))))

#_(defn process-push [form]
  ((push-fn form) form))


;;
;; Handlers
;;

(defstruct verb :word :default-arg-count :process)

(defn make-verb [& args]
  (with-meta (apply struct-map verb args)
             {:type ::verb}))

(defmethod print-method ::verb [obj writer]
  (cl-format writer "#<verb ~a>" (:word obj)))

(def assign         (make-verb :word "set"     :default-arg-count 3 :process process-assign))
(def collect        (make-verb :word "collect" :default-arg-count 2 :process process-collect))
(def concatentation (make-verb :word "concat"  :default-arg-count 2 :process process-concatenation))
(def conditional    (make-verb :word "if"      :default-arg-count 4 :process process-conditional))
(def seq-iteration  (make-verb :word "for"     :default-arg-count 3 :process process-seq-iteration))
(def sum            (make-verb :word "sum"     :default-arg-count 2 :process process-sum))


(def *handler-table* (atom {}))

(defn register [& handlers]
  (swap! *handler-table*
         (fn [handler-table]
           (reduce (fn [accum verb]
                     (assoc accum (:word verb) verb))
                   handler-table
                   handlers))))

(register assign collect concatentation conditional seq-iteration sum)

(defmulti find-handler (fn [handler-table form]
                         (cond (or (list?   form)
                                   (vector? form))
                               :sequence
                               (or (symbol?  form)
                                   (keyword? form))
                               :symbol)))

(defmethod find-handler :default [handler-table thing]
  (throw (Exception.
          (cl-format nil "cl-loop: Unrecognizable form: ~S~%  Expecting a symbol, keyword, list or vector." thing))))

(defmethod find-handler :symbol [handler-table word]
  (if-let [handler (handler-table (name word))]
    handler
    (throw (Exception. (str "cl-loop: Unknown clause " word)))))

(defmethod find-handler :sequence [handler-table sequence]
  (if-let [handler (handler-table (name (first sequence)))]
    handler
    (throw (Exception. (str "cl-loop: Unknown clause: " sequence)))))


(defn normalize-forms [init-forms handler-table]
  (loop [forms init-forms accum []]
    (if (seq forms)
      (let [form (first forms)]
        (cond (or (vector? form)
                  (list?   form))
              (recur (rest forms) (conj accum form))
              (or (keyword? form)
                  (symbol?  form))
              (let [word      form
                    handler   (find-handler handler-table word)
                    num-args  (:default-arg-count handler)
                    next-form (take num-args forms)
                    next-form (into [] next-form)]
                (if (not= num-args (count next-form))
                  (throw (Exception. (cl-format nil "cl-loop: Not enough terms to complete '~S' clause: ~S" word next-form)))
                  (recur (drop num-args forms) (conj accum next-form))))
              true
              (throw (Exception. (cl-format nil "cl-loop: Unrecognizable form: ~S~%  Expecting a symbol, keyword, list or vector." form)))))
      accum)))

(defn process-form [handler-table normalized-form]
  (let [handler (find-handler handler-table normalized-form)
        processing-fn (:process handler)]
    (processing-fn normalized-form)))

(defmacro cl-loop [& forms]
  (let [handler-table     @*handler-table*
        normalized-forms  (normalize-forms forms handler-table)
        processed-forms   (map (partial process-form handler-table) normalized-forms)
        merged-processing (reduce (fn [accum processed]
                                    (merge-with conj accum processed))
                                  {:init  []
                                   :bind  []
                                   :test  []
                                   :push  []
                                   :recur []}
                                  processed-forms)
        inits  (reduce concat [] (:init merged-processing))
        binds  (reduce concat [] (:bind merged-processing))
        tests  (:test merged-processing)
        pushes (:push merged-processing)
        recurs (:recur merged-processing)]
    `(let [~'default-collector (atom nil)]
       (loop [~@inits]
         (if (and ~@tests)
           (let [~@binds]
             ~@pushes
             (recur ~@recurs))
           (deref ~'default-collector))))))



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