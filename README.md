# Rolling Stones

*"I can't get no..."*

Rolling Stones is a Clojure interface to the Sat4j satisfaction solver.

A satisfaction solver uses a combination of brute force searching and clever heuristics to determine whether a given Boolean formula is satisfiable, that is, whether an assignment of truth values for the Boolean variables exists that causes the entire formula to be true.

Satisfiability (aka SAT) was the first computer science problem to be proven NP-complete.  Most other NP-complete problems are proven NP-complete by showing that a reduction to SAT exists.  This makes a SAT solver an especially powerful tool, because many interesting problems (NP-complete and otherwise) can be encoded as a SAT problem.  SAT solving heuristics are an area of intensive study in academic research, and recent advancements in SAT solvers allow us to crack many classes of moderately complex SAT problems in short order.

## Usage

Rolling Stones currently requires Clojure 1.9 alpha 9 or higher due to a dependency on the new spec library.  I may eventually move the specs to a separate namespace to make this compatible with older versions of Clojure.

```
[rolling-stones "1.0.0-SNAPSHOT"]

(require '[rolling-stones.core :as sat :refer [!]])
```

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

But remember, you can use any Clojure data you want to denote variables. You don't need to restrict yourself to keywords.  As a somewhat crazy example, let's replace `:p` with `{:name "Bob"}`, `:q` with `[2 6]` and `:r` with `#{4}`.

```clojure
=> (sat/solve-symbolic-formula [[{:name "Bob"}] [{:name "Bob"} [2 6]]
                                [(! {:name "Bob"}) [2 6] (! #{4})]])
[[{:name "Bob"}] [{:name "Bob"} [2 6]] [(! {:name Bob}) [2 6] (! #{4})]]
```

Behind the scenes, Rolling Stones is simply translating each distinct Clojure value to a distinct integer (negative integer if wrapped in a `!`) and passing it to the integer variable solver discussed above, then translating the output back into the Clojure values you used.

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

Behind the scenes, Rolling Stones is expanding the formula into conjunctive normal form by way of something called Tseitin encoding, which introduces a temporary variable for each sub-expression in the formula.  It solves the expression using the above solver, and then reports back the values of all the original variables (ignoring the temporary variables it created).

As you can see, the combination of the speedy underlying Sat4j Java-based solver and the high-level ease of manipulating symbolic data via Clojure makes for a very powerful combination.

### Working with the output

Now that you've solved your logical formula, what do you do with the output?  There are two types of formulas supported by Rolling Stones -- those where the variables are represented by integers, and those where the variables are represented symbolically by arbitrary data.  Let's look at each of these formula types in turn.

#### Integer variable solutions

The output from `solve`, the solver for integer variable CNF inputs, will be a vector of numbers, positive for true and negative for false, e.g., `[1 -2 -3]`.

So Clojure's `pos?` predicate is our tool for testing for a true variable, `neg?` is our tool for testing for a false variable, and `-` is the way we negate variables, changing true to false and false to true.  This gives us everything we need to manipulate the result.

Examples:

```clojure
; Get all true variables
=> (filter pos? [1 -2 -3])

; Get all false variables
=> (filter neg? [1 -2 -3])

; Get all false variables, drop the minus sign
=> (map - (filter neg? [1 -2 -3]))

; Swap true and false variables
=> (mapv - [1 -2 -3])

; Collect all the true variables into a set
=> (def true-set (into {} (filter pos?) [1 -2 -3]))

; Test if a specific variable is true
=> (contains? true-set 1)
true
=> (contains? true-set 2)
false
```

So you can do whatever you want to the result, but personally I find that 90% of the time I want to build up a set of all the true variables so I can query against it.  So I've added a convenience function to do just that, namely `true-integer-variables`.  So the above example could be rewritten as:

```clojure
(def true-set (true-integer-variables [1 -2 -3]))
```

This isn't much more concise that just explicitly using `(into {} (filter pos?) ...)`, but it's there if you want to use it.

#### Symbolic variable solutions

When you use one of the symbolic solves, you get back a result like `[:p (! :q) (! :r)]`.  What exactly does this mean?  Specifically, how is a not like `(! q)` represented?

Internally, `!` is just a constructor that builds a Not record, and I've tweaked the way it prints.  If the printing weren't tweaked, the output would look like this:

```clojure
=> (! :x)
#rolling_stones.core.Not{:literal :x}
```

So as you can see, the contents of the Not are stored in a field called `:literal`, so if you need to extract the contents out of a Not, that's one way to do it (we'll see another way in a moment that doesn't require knowing the internal representation of Not).

It is convenient to extend the metaphor of integer variables to symbolic variables, and we can think of true variables as "positive" and false variables as "negative".  So, for symbolic variables, Rolling Stones provides the predicates `positive?` and `negative?` to test symbolic variables, and `negate` which swaps positive with negative and vice versa.

Examples:

```clojure
=> (sat/positive? :x)
true
=> (sat/negative? :x)
false
=> (sat/positive? (! :x))
false
=> (sat/negative? (! :x))
true
=> (sat/negate :x)
(! :x)
=> (sat/negate (! :x))
:x

; ! just builds a Not record, so doesn't have a notion of double negative
=> (! (! :x))
(! (! :x))

; But negate does
=> (sat/negate (sat/negate :x))
:x
```

As demonstrated above, negate is another way to extract the contents of a Not, without needing to know the internal representation of a Not.

With these tools in place, you can do exactly the same sorts of manipulations that we did with integer variable solutions:

```clojure
; Get all true variables
=> (filter sat/positive? [:p (! :q) (! :r)])

; Get all false variables
=> (filter sat/negative? [:p (! :q) (! :r)])

; Get all false variables, drop the minus sign
=> (map sat/negate (filter sat/negative? [:p (! :q) (! :r)]))

; Swap true and false variables
=> (mapv sat/negate [:p (! :q) (! :r)])

; Collect all the true variables into a set
=> (def true-set (into {} (filter sat/positive?) [:p (! :q) (! :r)]))

; Test if a specific variable is true
=> (contains? true-set :p)
true
=> (contains? true-set :q)
false
```

As in the integer case, a convenience function `true-symbolic-variables` is provided to build a set of true variables, since that is the most common need:

```clojure
(def true-set (true-symbolic-variables [:p (! :q) (! :r)]))
```

## API Summary

* Integer Variables, Conjunctive Normal Form
  + `solve`
  + `solutions`
* Symbolic Variables, Conjunctive Normal Form
  + `solve-symbolic-cnf`
  + `solutions-symbolic-cnf`
* Symbolic Variables, any logic formula
  + `solve-symbolic-formula`
  + `solutions-symbolic-formula`
* Manipulating integer solutions
  + Use clojure.core's `pos?`, `neg?`, and `-`
  + `true-integer-variables` builds a set of the true integer variables, i.e., the positive integers
* Manipulating symbolic solutions
  + Use rolling-stones.core's `positive?`, `negative?`, and `negate`
  + `true-symbolic-variables` builds a set of the true symbolic variables, i.e., any Clojure data that isn't wrapped in a Not.

## Roadmap

As far as my own personal needs go, this project is feature complete.  However, Sat4j has a ton of extra functionality that isn't exposed here, for example, it can solve MAXSAT, Pseudo-Boolean problems, and MUS problems, and there are a bunch of alternative solving algorithms as well as tools that can read formulas out of files in a variety of standardized formats.  I would welcome pull requests that expose more aspects of the underlying solver in a similarly Clojure-friendly way.

## License

Copyright © 2016 Mark Engelberg

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

