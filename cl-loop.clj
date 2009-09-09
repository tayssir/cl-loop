(use 'clojure.contrib.pprint)
(use 'clojure.contrib.def)

(def *default-collector*)

(defn process-assign [form]
  (let [[_ var val] form]
    {:bind `(~var ~val)}))

(defn process-collect [form]
  (let [[_ var] form]
    {:push `(set! *default-collector*
                  (conj *default-collector* ~var))}))

(defn process-concatenation [form]
  (let [[_ var] form]
    {:push `(set! *default-collector*
                  (into [] (concat *default-collector* ~var)))}))

(defn process-seq-iteration [form]
  (let [[prefix var list-form] form
        a-gensym (gensym "seq-iter__")]
    {:init  `(~a-gensym (seq ~list-form))
     :test  `(seq ~a-gensym)
     :bind  `(~var (first ~a-gensym))
     :recur `(rest ~a-gensym)}))

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


(def *handler-table* (atom {}))

(defn register [& handlers]
  (swap! *handler-table*
         (fn [handler-table]
           (reduce (fn [accum verb]
                     (assoc accum (:word verb) verb))
                   handler-table
                   handlers))))

(register assign collect concatentation conditional seq-iteration)

(defmulti find-handler (fn [handler-table form]
                         (cond (or (list?   form)
                                   (vector? form))
                               :sequence
                               (symbol? form)
                               :symbol)))

(defmethod find-handler :symbol [handler-table word]
  ;; fixme: signal error
  (handler-table (name word)))

(defmethod find-handler :sequence [handler-table vector]
  ;; fixme: signal error
  (handler-table (name (first vector))))


(defn normalize-forms [init-forms handler-table]
  ;; fixme: no unexpected # of forms
  ;; fixme: expect clauses to begin with vector or symbol
  (loop [forms init-forms accum []]
    (if (seq forms)
      (let [form (first forms)]
        (cond (or (vector? form)
                  (list?   form))
              (recur (rest forms) (conj accum form))
              (symbol? form)
              (let [word      form
                    handler   (find-handler handler-table word)
                    num-args  (:default-arg-count handler)
                    next-form (take num-args forms)
                    next-form (into [] next-form)]
                (recur (drop num-args forms) (conj accum next-form)))
              true (throw (Exception. (print "Unrecognizable form: " form)))))
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