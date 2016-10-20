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

(defn- find-model [^ISolver solver]
  (try
    (.findModel solver)
    (catch TimeoutException e nil)))

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
  ([clauses] (solve clauses nil))
  ([clauses timeout]
   (when-let [solver (create-solver clauses)]
     (when timeout
       (.setTimeoutMs solver timeout))
     (when-let [solution (find-model solver)]
       (with-meta (vec solution) (stats-map solver))))))

(defn- find-next-model [^ISolver solver]
  (try
    (when (.isSatisfiable solver)
      (.model solver))
    (catch TimeoutException e nil)))

(s/fdef solutions
        :args (s/cat :clauses (s/coll-of (s/or :constraint ::constraint
                                               :clause ::numeric-clause)
                                         :into ())
                     :timeout (s/? pos-int?))
        :ret (s/* ::numeric-clause))

(defn solutions 
  ([clauses] (solutions clauses nil))
  ([clauses timeout]
   (b/cond
     :when-let [solver (create-solver clauses)]
     :let [iterator (ModelIterator. solver)
           _ (when timeout (.setTimeoutMs solver timeout))
           solution-iterator (fn [_]
                               (when-let [next-solution (find-next-model iterator)]
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
  ([clauses] (solve-symbolic-cnf clauses nil))
  ([clauses timeout]
   (b/cond
     :let [[object->int int->object] (build-transforms clauses)
           transformed-clauses (mapv (clause-transformer object->int) clauses)]
     :when-let [solver (create-solver transformed-clauses)]
     :let [_ (when timeout (.setTimeoutMs solver timeout))]
     :when-let [solution (find-model solver)]
     :let [untransformed-solution ((clause-transformer int->object) solution)]
     (with-meta (vec untransformed-solution) (stats-map solver)))))

(s/fdef solutions-symbolic-cnf
        :args (s/cat :clauses (s/coll-of (s/or :clause ::symbolic-clause
                                               :constraint ::constraint)
                                         :into ()))
              :timeout (s/? pos-int?)
        :ret (s/* ::symbolic-clause))

(defn solutions-symbolic-cnf
  ([clauses] (solutions-symbolic-cnf clauses nil))
  ([clauses timeout]
   (b/cond
     :let [[object->int int->object] (build-transforms clauses)
           transformed-clauses (mapv (clause-transformer object->int) clauses)]
     :when-let [solver (create-solver transformed-clauses)]
     :let [iterator (ModelIterator. solver)
           _ (when timeout (.setTimeoutMs solver timeout)),
           solution-iterator (fn [_]
                               (when-let [next-solution (find-next-model iterator)]
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
    (let [literal (:literal this),
          [v clauses] (encode-cnf literal)
          not-v (gensym "temp")]
      [not-v (into clauses [[(! v) (! not-v)]
                            [v not-v]])]))
  And
  (encode-cnf [this]
    (let [literals (:literals this),
          vs-clauses (map encode-cnf literals),
          vs (map first vs-clauses)
          clauses (apply concat (map second vs-clauses)),
          and-v (gensym "temp")]
      [and-v (into clauses (cons
                             (vec (cons and-v (map ! vs)))
                             (for [v vs] [(! and-v) v])))]))
  Or
  (encode-cnf [this]
    (let [literals (:literals this),
          vs-clauses (map encode-cnf literals),
          vs (map first vs-clauses)
          clauses (apply concat (map second vs-clauses)),
          or-v (gensym "temp")]
      [or-v (into clauses (cons
                            (vec (cons (! or-v) vs))
                            (for [v vs] [or-v (! v)])))]))  
  Xor
  (encode-cnf [this]
    (let [literal1 (:literal1 this),
          literal2 (:literal2 this),
          [v1 clauses1] (encode-cnf literal1)
          [v2 clauses2] (encode-cnf literal2)
          xor-v (gensym "temp")]
      [xor-v (into (concat clauses1 clauses2)
                   [[(! xor-v) (! v1) (! v2)]
                    [xor-v v1 (! v2)]
                    [xor-v (! v1) v2]
                    [(! xor-v) v1 v2]])]))  
  Imp
  (encode-cnf [this]
    (let [literal1 (:literal1 this),
          literal2 (:literal2 this),
          [v1 clauses1] (encode-cnf literal1)
          [v2 clauses2] (encode-cnf literal2)
          imp-v (gensym "temp")]
      [imp-v (into (concat clauses1 clauses2)
                   [[(! imp-v) (! v1) v2]
                    [imp-v v1]
                    [imp-v (! v2)]])]))
  
  Iff
  (encode-cnf [this]
    (let [literal1 (:literal1 this),
          literal2 (:literal2 this),
          [v1 clauses1] (encode-cnf literal1)
          [v2 clauses2] (encode-cnf literal2)
          iff-v (gensym "temp")]
      [iff-v (into (concat clauses1 clauses2)
                   [[(! iff-v) v1 (! v2)]
                    [iff-v (! v1) (! v2)]
                    [iff-v v1 v2]
                    [(! iff-v) (! v1) v2]])])))
  
(defn formula->cnf [wff]
  (let [[v clauses] (encode-cnf wff)]
    (conj clauses [v])))

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
  ([wffs] (solve-symbolic-formula wffs nil))
  ([wffs timeout]
   (b/cond
     (sequential? wffs)
     (let [{constraints true, wffs false} (group-by constraint? wffs)
           cnf (into [] (comp (map formula->cnf) cat) wffs)
           clauses (into cnf constraints),
           solution (solve-symbolic-cnf clauses timeout)]
      (filterv (complement temporary?) solution))  
     :else (recur [wffs] timeout))))

(s/fdef solutions-symbolic-formula
        :args (s/cat :formula-or-formulas
                     (s/alt :single-formula ::symbolic-formula
                            :multiple-formulas (s/coll-of (s/or :formula ::symbolic-formula
                                                                :constraint ::constraint)
                                                          :into ()))
                     :timeout (s/? pos-int?))
        :ret (s/* ::symbolic-clause))

(defn solutions-symbolic-formula
  ([wffs] (solutions-symbolic-formula wffs nil))
  ([wffs timeout]
   (b/cond
     (sequential? wffs)
     (let [{constraints true, wffs false} (group-by constraint? wffs)
           cnf       (into [] (comp (map formula->cnf) cat) wffs)
           clauses   (into cnf constraints),
           solutions (solutions-symbolic-cnf clauses timeout)]
       (for [solution solutions]
         (with-meta (filterv (complement temporary?) solution) (meta solution))))
     :else (recur [wffs] timeout))))

; Tools for working with symbolic variables
(def positive? (complement not?))
(def negative? not?)
(defn negate [x] (if (not? x) (:literal x) (NOT x)))

(defn true-integer-variables
  "Returns a set of all the true variables from a collection of integer variables"
  [coll]
  (into #{} (filter pos?) coll))

(defn true-symbolic-variables
  "Returns a set of all the true variables from a collection of symbolic variables"
  [coll]
  (into #{} (remove not?) coll))