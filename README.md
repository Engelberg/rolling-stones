# Rolling Stones

*"I can't get no..."*

Rolling Stones is a Clojure interface to the Sat4j satisfaction solver.

A satisfaction solver uses a combination of brute force searching and clever heuristics to determine whether a given Boolean formula is satisfiable, that is, whether an assignment of truth values for the Boolean variables exists that causes the entire formula to be true.

Satisfiability (aka SAT) was the first computer science problem to be proven NP-complete.  Most other NP-complete problems are proven NP-complete by showing that a reduction to SAT exists.  This makes a SAT solver an especially powerful tool, because many interesting problems (NP-complete and otherwise) can be encoded as a SAT problem.  SAT solving heuristics are an area of intensive study in academic research, and recent advancements in SAT solvers allow us to crack many classes of moderately complex SAT problems in short order.

## Usage

Rolling Stones currently requires Clojure 1.9 alpha 9 or higher due to a dependency on the new spec library.  I may eventually move the specs to a separate namespace to make this compatible with older versions of Clojure.

[rolling-stones "1.0.0-SNAPSHOT"]

(require '[rolling-stones.core :as sat :refer [!]])

### Conjunctive Normal Form

The most efficient way to invoke the SAT solver is to hand it a problem that is already in *conjunctive normal form*, i.e., it is an AND of a bunch ORs.  Each OR is comprised of only plain variables and the NOT of plain variables.

Example of Conjunctive Normal Form (written in infix notation):

p AND (p OR q) AND ((NOT p) OR q OR (NOT r))

SAT4j expects us to use integers (starting from 1) for our variables, and negative integers represent the NOT of a variable.  If we let 1 stand for p, 2 stand for q, and 3 stand for r, the above formula would be expressed in Clojure as:

`[[1] [1 2] [-1 2 -3]]`

In other words, we create a vector of vectors.  Each inner vector represents an OR clause, and the outer vector represents the overall AND.

We can solve this as follows:

```clojure
=> (sat/solve [[1] [1 2] [-1 2 -3]])
[1 -2 -3]
```

This answer tells us that if variable 1 (p) is set to true and variables 2 and 3 (q and r) are set to false, the overall formula will be true.  We can get all the true combinations as follows:

```clojure
=> (sat/solutions [[1] [1 2] [-1 2 -3]])
([1 -2 -3] [1 2 -3] [1 2 3])
```

Both functions take an optional second argument, the timeout in milliseconds.

Unsatisfiable formulas return nil:

```clojure
=> (solve [[1] [-1]])
nil
```

The underlying SAT solver, Sat4j, is especially interesting because it supports additional *constraints* in which you specify that at least, at most, or exactly some number of a set of variables must be true.

Examples of constraints:

`(exactly 2 [1 2 3])` - exactly 2 of the variables 1, 2, 3 must be true

`(at-least 3 [1 -2 3 -4 5])` - at least 3 of 1, -2, 3, -4, 5 must be true

`(at-most 1 [-1 -2 -3])` - at most 1 of -1, -2, -3 must be true

These constraints can be included inside the outer vector.

```clojure
=> (sat/solutions [[-1 2 -3] [1 2 3 4 5] (at-least 3 [2 3 4 5])])
([-1 2 3 4 -5] [-1 2 3 -4 5] [-1 2 -3 4 5] [-1 -2 3 4 5] [-1 2 3 4 5] [1 2 3 -4 5] [1 2 3 4 -5] [1 2 -3 4 5] [1 2 3 4 5])
```

### Symbolic Conjunctive Normal Form

The underlying Java solver wants the formula in terms of numbers, but in Clojure we're used to working at a higher-level and we want to be able to express variables with arbitrary Clojure data structures, maybe symbols, keywords, numbers, vectors, records, etc.

Rolling Stones lets you use anything you want as variables by calling the more versatile functions `solve-symbolic-cnf` or `solutions-symbolic-cnf`.  Again our input is a vector of vectors, with the inner vectors representing OR, and the outer vector representing AND.  Not is expressed by calling the function `!`.

For the purposes of our example, let's use keywords for our variables (although any valid Clojure data would work).

```clojure
=> (sat/solve-symbolic-cnf [[:p] [:p :q] [(! :p) :q (! :r)]])
[:p (! :q) (! :r)]
=> (sat/solutions-symbolic-cnf [[:p] [:p :q] [(! :p) :q (! :r)]])
([:p (! :q) (! :r)] [:p :q (! :r)] [:p :q :r])
=> (sst/solutions-symbolic-cnf [[:p] [:p :q] [(! :p) :q (! :r)]
                                (at-least 2 [:p :q :r])])
([:p :q (! :r)] [:p :q :r])
```

### Symbolic formulas

If you don't have your logical formula in conjunctive normal form, you can build it using the functions AND, OR, NOT (which is just an alias for !), XOR, IFF (if and only if, aka biconditional), IMP (for implies, aka conditional, e.g., P -> Q), NOR, and NAND -- Rolling Stones will convert the formula to CNF for you.  These are Clojure functions, so we express our formulas with prefix notation.  AND, OR, NOR, and NAND take any number of inputs, NOT of course takes only one input, and the rest take exactly two inputs.

The functions `solve-symbolic-formula` and `solutions-symbolic-formula` can either take a single formula:

```clojure
(require '[rolling-stones.core :as sat :refer [! NOT AND OR XOR IFF IMP NOR NAND]])

=> (sat/solve-symbolic-formula (XOR (AND :p :q (! :r)) (IFF :p (IMP :q :r))))
[:q (! :p) (! :r)]
=> (sat/solutions-symbolic-formula (XOR (AND :p :q (! :r)) (IFF :p (IMP :q :r))))
([:q (! :p) (! :r)] [:q :p (! :r)] [(! :q) :p (! :r)] [(! :q) :p :r] [:q :p :r])
```

or a sequence of formulas and/or constraints which are implicitly joined with AND:

```clojure
=> (sat/solve-symbolic-formula [(XOR :p :q) (NAND :q :r) (at-most 1 [:p :q :r])])
[:p (! :q) (! :r)]
```

The combination of the speedy underlying Sat4j Java-based solver and the high-level ease of manipulating symbolic data via Clojure makes for a very powerful combination.

## Roadmap

As far as my own personal needs go, this project is feature complete.  However, Sat4j has a ton of extra functionality that isn't exposed here, for example, it can solve MAXSAT, Pseudo-Boolean problems, and MUS problems, and there are a bunch of alternative solving algorithms as well as tools that can read formulas out of files in a variety of standardized formats.  I would welcome pull requests that expose more aspects of the underlying solver in a similarly Clojure-friendly way.

When reading the source code and creating pull requests, please note that Rolling Stones uses the [better-cond library](https://github.com/Engelberg/better-cond) which permits :let clauses in the cond.

## License

Copyright © 2016 Mark Engelberg

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
