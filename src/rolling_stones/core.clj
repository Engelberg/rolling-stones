(ns rolling-stones.core)
  (:require [better-cond.core :as b])
  (:import [org.sat4j.core VecInt]
           org.sat4j.minisat.SolverFactory
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
        max-var (apply max (map abs (apply concat clauses)))]
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

(defn solve 
  ([clauses] (solve clauses nil))
  ([clauses timeout]
    (when-let [solver (create-solver clauses)]
      (when timeout 
        (.setTimeoutMs solver timeout))
      (when-let [solution (.findModel solver)]
        (vec solution)))))

(defn- find-next-model [^ISolver solver]
  (try
    (when (.isSatisfiable solver true) ; true means don't reset timer
      (.model solver))
    (catch TimeoutException e nil)))

(defn solutions 
  ([clauses] (solutions clauses nil))
  ([clauses timeout]
    (b/cond
      :when-let [solver (create-solver clauses)]
      :let [iterator (ModelIterator. solver)
            _ (when timeout (.setTimeoutMs solver timeout))
            solution-iterator (fn [sol]
                                (when-let [next-solution (find-next-model iterator)]
                                  next-solution))]
      :when-let [first-solution (find-next-model iterator)]
      (map vec (take-while identity (iterate solution-iterator first-solution))))))          

(defn- make-counter "Makes counter starting from n" [n] 
  (let [ctr (atom (dec n))]
    (memoize (fn [x] (swap! ctr inc))))) 

(defrecord Not [literal])
(def ! ->Not)
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
      (mapv transform clause))))

(defn solve-general 
  ([clauses] (solve-general clauses nil))
  ([clauses timeout]
    (b/cond
      :let [[object->int int->object] (build-transforms clauses)
            transformed-clauses (mapv (clause-transformer object->int) clauses)]
      :when-let [solver (create-solver transformed-clauses)]
      :let [_ (when timeout (.setTimeoutMs solver timeout))]
      :when-let [solution (.findModel solver)]
      :let [untransformed-solution ((clause-transformer int->object) solution)] 
      (vec untransformed-solution))))

(defn solutions-general 
  ([clauses] (solutions-general clauses nil))
  ([clauses timeout]
    (b/cond
      :let [[object->int int->object] (build-transforms clauses)
            transformed-clauses (mapv (clause-transformer object->int) clauses)]
      :when-let [solver (create-solver transformed-clauses)]
      :let [iterator (ModelIterator. solver)
            _ (when timeout (.setTimeoutMs solver timeout)),
            solution-iterator (fn [sol]
                                (when-let [next-solution (find-next-model iterator)]
                                  next-solution))]
      :when-let [first-solution (find-next-model iterator)]        
      (map (clause-transformer int->object) 
           (map vec 
                (take-while identity (iterate solution-iterator first-solution)))))))


;TBD Add support for logic formulas that are not in CNF form, see org.sat4j.tools/GateTranslator
;Tseitin encodings