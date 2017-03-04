(ns rolling-stones.core
  (:require [better-cond.core :as b]
            [clojure.spec :as s]
            [clojure.pprint])
  (:import [org.sat4j.core VecInt]
           org.sat4j.minisat.SolverFactory
           org.sat4j.minisat.core.SolverStats
           org.sat4j.tools.ModelIterator
           [org.sat4j.specs ContradictionException TimeoutException ISolver IProblem]))

(defn- abs [x] (if (neg? x) (- x) x))

(defn- vec-int [v]
  (VecInt. (int-array v)))

(defrecord Constraint [type n literals])
(defn at-most [n literals] (->Constraint :at-most n literals))
(defn at-least [n literals] (->Constraint :at-least n literals))
(defn exactly [n literals] (->Constraint :exactly n literals))
(defn constraint? [c] (instance? Constraint c))

(defn- add-at-most [^ISolver solver literals n]
  (.addAtMost solver (vec-int literals) n))

(defn- add-at-least [^ISolver solver literals n]
  (.addAtLeast solver (vec-int literals) n))

(defn- add-exactly [^ISolver solver literals n]
  (.addExactly solver (vec-int literals) n))

(defn- add-constraint [solver constraint]
  ((case (:type constraint)
     :at-most add-at-most
     :at-least add-at-least
     :exactly add-exactly)
   solver (:literals constraint) (:n constraint)))

(defn- create-solver ^ISolver [clauses]
  (let [^ISolver solver (SolverFactory/newDefault),
        {constraints true, clauses false} (group-by constraint? clauses)        
        max-var (max (transduce (comp cat (map abs)) max 1 clauses)  
                     (transduce (comp (map :literals) cat (map abs)) max 1 constraints))]
    (.newVar solver max-var)
    (.setExpectedNumberOfClauses solver (count clauses))
    (try 
      (dotimes [i (count clauses)]
        (.addClause solver (vec-int (clauses i))))
      (dotimes [i (count constraints)]
        (add-constraint solver (constraints i)))
      solver
      (catch ContradictionException e nil))))

(defn- set-timeout-atom! [timeout-atom]
  (when timeout-atom (do (reset! timeout-atom true) nil)))

(defn- find-model [^ISolver solver timeout-atom]
  (try
    (.findModel solver)
    (catch TimeoutException e (set-timeout-atom! timeout-atom))))

(defn- stats-map [^ISolver solver]
  (let [^SolverStats stats (.getStats solver)]
    {:starts (.starts stats),
     :decisions (.decisions stats),
     :propagations (.propagations stats),
     :inspects (.inspects stats),
     :shortcuts (.shortcuts stats),
     :conflicts (.conflicts stats),
     :learned-literals (.learnedliterals stats),
     :learned-clauses (.learnedclauses stats),
     :ignored-clauses (.ignoredclauses stats),
     :learned-binary-clauses (.learnedbinaryclauses stats),
     :learned-ternary-clauses (.learnedternaryclauses stats),
     :root-simplifications (.rootSimplifications stats),
     :reduced-literals (.reducedliterals stats),
     :changed-reason (.changedreason stats),
     :reduced-db (.reduceddb stats),
     :update-lbd (.updateLBD stats),
     :imported-units (.importedUnits stats)}))
  

(s/def ::constraint constraint?)
(s/def ::numeric-clause (s/coll-of (s/and int? #(not= % 0))
                                   :into []))
(s/fdef solve
        :args (s/cat :clauses (s/coll-of (s/or :constraint ::constraint
                                               :clause ::numeric-clause)
                                         :into ())
                     :timeout (s/? pos-int?))
        :ret (s/nilable ::numeric-clause))

(defn solve
  "Takes a formula in integer CNF form, i.e., vector of constraints and vectors of non-zero ints
  and optional timeout in millis and optional atom to set to true if timeout occurs.
  Returns solution or nil if no solution exists.  Stats attached as metadata."
  ([clauses] (solve clauses nil nil))
  ([clauses timeout] (solve clauses timeout nil))
  ([clauses timeout timeout-atom]
   (when-let [solver (create-solver clauses)]
     (when timeout
       (.setTimeoutMs solver timeout))
     (when-let [solution (find-model solver timeout-atom)]
       (with-meta (vec solution) (stats-map solver))))))

(defn- find-next-model [^ISolver solver timeout-atom]
  (try
    (when (.isSatisfiable solver)
      (.model solver))
    (catch TimeoutException e (set-timeout-atom! timeout-atom))))

(s/fdef solutions
        :args (s/cat :clauses (s/coll-of (s/or :constraint ::constraint
                                               :clause ::numeric-clause)
                                         :into ())
                     :timeout (s/? pos-int?))
        :ret (s/* ::numeric-clause))

(defn solutions
  "Takes a formula in integer CNF form, i.e., vector of constraints and vectors of non-zero ints
  and optional timeout in millis and optional atom to set to true if timeout occurs.
  Returns lazy sequence of all solutions.  Stats attached as metadata to each solution."
  ([clauses] (solutions clauses nil nil))
  ([clauses timeout] (solutions clauses timeout nil))
  ([clauses timeout timeout-atom]
   (b/cond
     :when-let [solver (create-solver clauses)]
     :let [iterator (ModelIterator. solver)
           _ (when timeout (.setTimeoutMs solver timeout))
           solution-iterator (fn [_]
                               (when-let [next-solution (find-next-model iterator timeout-atom)]
                                 (with-meta (vec next-solution) (stats-map solver))))]
     :when-let [first-solution (solution-iterator nil)]
     (take-while identity (iterate solution-iterator first-solution)))))

(defn- make-counter "Makes counter starting from n" [n] 
  (let [ctr (atom (dec n))]
    (memoize (fn [x] (swap! ctr inc))))) 

(defrecord Not [literal])
(def ! ->Not)
(def NOT ->Not)
(defn not? [r] (instance? Not r))

(defmethod clojure.core/print-method Not [x writer]
  (binding [*out* writer]
    (print "(! ")
    (print (:literal x))
    (print ")")))

(. clojure.pprint/simple-dispatch addMethod Not #(clojure.core/print-method % *out*))

(defn- make-id-generator []
  (let [id (make-counter 1)]
    (fn [x]
      (cond 
        (instance? Not x) (- (id (:literal x)))
        :else (id x)))))        

(defn- all-literals [clauses]
  (into [] (comp 
             (mapcat (fn [clause] (if (constraint? clause) (:literals clause) clause)))
             (map (fn [l] (if (not? l) (:literal l) l)))
             (distinct))
        clauses))

(defn- build-transforms [clauses]
  (let [literals (all-literals clauses)
        object->int (into {} (for [i (range (count literals))]
                               [(literals i) (inc i)])),
        int->object (fn [i] (literals (dec i)))]
    [(fn [o] (if (not? o) (- (object->int (:literal o))) (object->int o)))
     (fn [i] (if (neg? i) (! (int->object (- i))) (int->object i)))]))

(defn- clause-transformer [transform]
  (fn [clause]
    (if (constraint? clause)
      (update clause :literals (partial mapv transform))
      (if-let [m (meta clause)]
        (with-meta (mapv transform clause) m)
        (mapv transform clause)))))

(s/def ::symbolic-clause (s/coll-of any? :into []))

(s/fdef solve-symbolic-cnf
        :args (s/cat :clauses (s/coll-of (s/or :clause ::symbolic-clause
                                               :constraint ::constraint) 
                                         :into ()))
              :timeout (s/? pos-int?)
        :ret (s/nilable ::symbolic-clause))

(defn solve-symbolic-cnf
  "Takes a formula in symbolic CNF form, i.e., vector of constraints and vectors of variables and
  ! of variables, where variables are represented by arbitrary Clojure data.
  Also takes optional timeout in millis and optional atom to set to true if timeout occurs.
  Returns solution or nil if no solution exists.  Stats attached as metadata."
  ([clauses] (solve-symbolic-cnf clauses nil nil))
  ([clauses timeout] (solve-symbolic-cnf clauses timeout nil))
  ([clauses timeout timeout-atom]
   (b/cond
     :let [[object->int int->object] (build-transforms clauses)
           transformed-clauses (mapv (clause-transformer object->int) clauses)]
     :when-let [solver (create-solver transformed-clauses)]
     :let [_ (when timeout (.setTimeoutMs solver timeout))]
     :when-let [solution (find-model solver timeout-atom)]
     :let [untransformed-solution ((clause-transformer int->object) solution)]
     (with-meta (vec untransformed-solution) (stats-map solver)))))

(s/fdef solutions-symbolic-cnf
        :args (s/cat :clauses (s/coll-of (s/or :clause ::symbolic-clause
                                               :constraint ::constraint)
                                         :into ()))
              :timeout (s/? pos-int?)
        :ret (s/* ::symbolic-clause))

(defn solutions-symbolic-cnf
  "Takes a formula in symbolic CNF form, i.e., vector of constraints and vectors of variables and
  ! of variables, where variables are represented by arbitrary Clojure data.
  Also takes optional timeout in millis and optional atom to set to true if timeout occurs.
  Returns lazy sequence of solutions.  Stats attached as metadata to each solution."
  ([clauses] (solutions-symbolic-cnf clauses nil nil))
  ([clauses timeout] (solutions-symbolic-cnf clauses timeout nil))
  ([clauses timeout timeout-atom]
   (b/cond
     :let [[object->int int->object] (build-transforms clauses)
           transformed-clauses (mapv (clause-transformer object->int) clauses)]
     :when-let [solver (create-solver transformed-clauses)]
     :let [iterator (ModelIterator. solver)
           _ (when timeout (.setTimeoutMs solver timeout)),
           solution-iterator (fn [_]
                               (when-let [next-solution (find-next-model iterator timeout-atom)]
                                 (with-meta (vec next-solution) (stats-map solver))))]
     :when-let [first-solution (solution-iterator nil)]
     (map (clause-transformer int->object)
          (take-while identity (iterate solution-iterator first-solution))))))


;Tseitin encodings

(defrecord And [literals])
(defn AND [& literals]
  (->And literals))
(defn and? [x]
  (instance? AND x))

(defrecord Or [literals])
(defn OR [& literals]
  (->Or literals))
(defn or? [x]
  (instance? Or x))

(defrecord Xor [literal1 literal2])
(def XOR ->Xor)
(defn xor? [x]
  (instance? Xor x))

(defrecord Imp [literal1 literal2])
(def IMP ->Imp)
(defn imp? [x]
  (instance? Imp x))

(defrecord Iff [literal1 literal2])
(def IFF ->Iff)
(defn iff? [x]
  (instance? Iff x))

(defn NOR [& literals]
  (NOT (apply OR literals)))

(defn NAND [& literals]
  (NOT (apply AND literals)))

(defn temporary? [var]
  (let [literal (if-let [literal (:literal var)] literal var)]
    (and (symbol? literal)
         (clojure.string/starts-with? (name literal) "temp"))))

(defn formula? [x]
  (contains? #{Not And Or Xor Imp Iff} (type x)))

(defprotocol CNF
  (encode-cnf [this] "Returns variable and clauses"))

(extend-protocol CNF
  Object
  (encode-cnf [this] [this []])
  Not
  (encode-cnf [this]
    (b/cond
      :let [literal (:literal this)]
      (not (formula? literal)) [this []]
      :let [[v clauses] (encode-cnf literal)
            not-v (gensym "temp")]
      [not-v (into clauses [[(negate v) (negate not-v)]
                            [v not-v]])]))
  And
  (encode-cnf [this]
    (let [literals (:literals this),
          vs-clauses (map encode-cnf literals),
          vs (map first vs-clauses)
          clauses (apply concat (map second vs-clauses)),
          and-v (gensym "temp")]
      [and-v (into clauses (cons
                             (vec (cons and-v (map negate vs)))
                             (for [v vs] [(negate and-v) v])))]))
  Or
  (encode-cnf [this]
    (let [literals (:literals this),
          vs-clauses (map encode-cnf literals),
          vs (map first vs-clauses)
          clauses (apply concat (map second vs-clauses)),
          or-v (gensym "temp")]
      [or-v (into clauses (cons
                            (vec (cons (negate or-v) vs))
                            (for [v vs] [or-v (negate v)])))]))
  Xor
  (encode-cnf [this]
    (let [literal1 (:literal1 this),
          literal2 (:literal2 this),
          [v1 clauses1] (encode-cnf literal1)
          [v2 clauses2] (encode-cnf literal2)
          xor-v (gensym "temp")]
      [xor-v (into (concat clauses1 clauses2)
                   [[(negate xor-v) (negate v1) (negate v2)]
                    [xor-v v1 (negate v2)]
                    [xor-v (negate v1) v2]
                    [(negate xor-v) v1 v2]])]))
  Imp
  (encode-cnf [this]
    (let [literal1 (:literal1 this),
          literal2 (:literal2 this),
          [v1 clauses1] (encode-cnf literal1)
          [v2 clauses2] (encode-cnf literal2)
          imp-v (gensym "temp")]
      [imp-v (into (concat clauses1 clauses2)
                   [[(negate imp-v) (negate v1) v2]
                    [imp-v v1]
                    [imp-v (negate v2)]])]))
  
  Iff
  (encode-cnf [this]
    (let [literal1 (:literal1 this),
          literal2 (:literal2 this),
          [v1 clauses1] (encode-cnf literal1)
          [v2 clauses2] (encode-cnf literal2)
          iff-v (gensym "temp")]
      [iff-v (into (concat clauses1 clauses2)
                   [[(negate iff-v) v1 (negate v2)]
                    [iff-v (negate v1) (negate v2)]
                    [iff-v v1 v2]
                    [(negate iff-v) (negate v1) v2]])])))
  
(defn formula->cnf [wff]
  (let [[v clauses] (encode-cnf wff)]
    (conj clauses [v])))

(defn- literal? "Is it a plain variable or a NOT of a plain variable?" [wff]
  (or (not (formula? wff)) (and (not? wff) (not (formula? (:literal wff))))))

(defn encode-constraint "Returns (cons new-constraint new-clauses)" [constraint]
  (loop [literals (seq (:literals constraint)), new-literals [], new-clauses []]
    (b/cond
      (not literals) (cons (assoc constraint :literals new-literals) new-clauses)
      :let [literal (first literals)]
      (literal? literal) (recur (next literals) (conj new-literals literal) new-clauses)
      :let [[new-literal clauses] (encode-cnf literal)]
      (recur (next literals) (conj new-literals new-literal) (into new-clauses clauses)))))

(s/def ::symbolic-formula formula?)

(s/fdef solve-symbolic-formula
        :args (s/cat :formula-or-formulas
                     (s/alt :single-formula ::symbolic-formula
                            :multiple-formulas (s/coll-of (s/or :formula ::symbolic-formula
                                                                :constraint ::constraint)
                                                          :into ()))
                     :timeout (s/? pos-int?))
        :ret (s/nilable ::symbolic-clause))

(defn solve-symbolic-formula
  "Takes a formula in symbolic form, i.e., vector of constraints and formulas, where formulas
  are built with formula constructors such as AND, OR, XOR, IFF, NAND, NOR, IMP, NOT,
  and formula variables are represented by arbitrary Clojure data.
  Also takes optional timeout in millis and optional atom to set to true if timeout occurs.
  Returns solution or nil if no solution exists.  Stats attached as metadata."
  ([wffs] (solve-symbolic-formula wffs nil nil))
  ([wffs timeout] (solve-symbolic-formula wffs timeout nil))
  ([wffs timeout timeout-atom]
   (b/cond
     (not (sequential? wffs)) (recur [wffs] timeout timeout-atom)
     :let [{constraints true, wffs false} (group-by constraint? wffs)
           cnf       (into [] (comp (map formula->cnf) cat) wffs)
           clauses   (into cnf (mapcat encode-constraint) constraints)]
     :when-let [solution (solve-symbolic-cnf clauses timeout timeout-atom)]
     (filterv (complement temporary?) solution))))

(s/fdef solutions-symbolic-formula
        :args (s/cat :formula-or-formulas
                     (s/alt :single-formula ::symbolic-formula
                            :multiple-formulas (s/coll-of (s/or :formula ::symbolic-formula
                                                                :constraint ::constraint)
                                                          :into ()))
                     :timeout (s/? pos-int?))
        :ret (s/* ::symbolic-clause))

(defn solutions-symbolic-formula
  "Takes a formula in symbolic form, i.e., vector of constraints and formulas, where formulas
  are built with formula constructors such as AND, OR, XOR, IFF, NAND, NOR, IMP, NOT,
  and formula variables are represented by arbitrary Clojure data.
  Also takes optional timeout in millis and optional atom to set to true if timeout occurs.
  Returns lazy sequence of all solutions.  Stats attached as metadata."
  ([wffs] (solutions-symbolic-formula wffs nil nil))
  ([wffs timeout] (solutions-symbolic-formula wffs timeout nil))
  ([wffs timeout timeout-atom]
   (b/cond
     (not (sequential? wffs)) (recur [wffs] timeout timeout-atom)
     :let [{constraints true, wffs false} (group-by constraint? wffs)
           cnf       (into [] (comp (map formula->cnf) cat) wffs)
           clauses   (into cnf (mapcat encode-constraint) constraints)]
     :when-let [solutions (solutions-symbolic-cnf clauses timeout timeout-atom)]
     (for [solution solutions]
       (with-meta (filterv (complement temporary?) solution) (meta solution))))))

; Tools for working with symbolic variables
(def positive? (complement not?))
(def negative? not?)
(defn negate [x] (if (not? x) (:literal x) (NOT x)))

(defn true-integer-variables
  "Returns a set of all the true variables from a collection of integer variables.
  Returns nil if passed nil."
  [coll]
  (into #{} (filter pos?) coll))

(defn true-symbolic-variables
  "Returns a set of all the true variables from a collection of symbolic variables.
  Returns nil if passed nil."
  [coll]
  (into #{} (remove not?) coll))