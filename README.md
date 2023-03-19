# ltl-translators

The output is always in a "prefix ltl" format, which is similar to the LISP syntax.
Example:

```
(& (! a1) (U t a1))
```

The supported operators are:
- n-ary
    - `&` conjunction
    - `|` disjunction
- binary
    - `U` until
    - `W` weak until
    - `G` globally
    - `F` finally
    - `R` release
- unary
    - `!` negation
    - `X` next
- nullary
    - `t` true
    - `f` false
    - `a0, a1, a2, ...` free variables

## ltl-randgen-exe

Generate random LTL. The generation is launched with a single nonterminal symbol N.
Then, nonterminals get rewritten left to right with one of the grammar rules:

  - `N -> (op N N)` where `op` is one of `& | U` (we will call these rules *binary*)
  - `N -> (op N)` where `op` is one of `! X` (we will call these rules *unary*)
  - `N -> a<i>` where `i` is in range `0..terminalCount-1` (we will call these
    rules *terminal*)

The unary and binary rules will be together called *nonterminal*.

TERMINOLOGY NOTE 1: By *tree* we mean a generated *grammar tree* of the above
grammar. The term *tree* and *grammar tree* will be used interchangeably.

TERMINOLOGY NOTE 2: We will use the terms *height* and *depth* of grammar trees
interchangeably, as trees are usually drawn upside down (root is at the top).

At each rewriting, each rule has its probability and the random roulette picks
one of them. Basically, there are three types of rewriting, among which we select by a
(later described) logic that ensures that the grammar tree satisfies configured
properties (height, etc). The three types of rewriting are the
following:

  - unconstrained rewriting - Any rule can be applied. Each of them has a
    hardcoded probability (see `weightsAll`).
  - terminal rewriting - Only terminal rules have nonzero probability. Currently,
    the only terminal rules are variable rules (see `weightsTerm`) and there is
    even probability distribution between the variables.
  - nonterminal rewriting - Only nonterminal rules have nonzero probability
    (see `weightsNonTerm`).

Currently hardcoded probabilities are results of our experimentation. We were
trying to generate such formulae, satisfiability of which is not always easy to
be checked by current model checkers.

Terminal rewriting is used to limit the height of the tree from top (as it
stops the generation of a branch). Nonterminal rewriting is used for the
opposite: forcing higher heights of the tree.

There are four properties that can be configured:

  - `terminalCount` - Number of possible variables.

  - `depth` - Height of the grammar tree. At least one path from root will be
    this long and no path will be longer. Specifically, we generate (almost)
    all paths using this parameter *only* as the upper limit, only if there is
    no path of the length `depth`, the rightmost path is forced to be exactly
    `depth` long.

  - `avgDepthDecay` - should be less than 1, see `avgDepth` for more detail.
    Smaller values of the decay cause greater unbalance of depths of left and
    right subtrees of binary operators.

  - `avgDepth` - This parameter will ensure some density of the grammar tree
    and more or less balanced depths of operands of binary nodes (balancing
    depends on the property `avgDepthDecay`).

    Very roughly, it is a *lower limit on the average depth of root's subtrees*
    but to be precise, it is more than that - shapes of subtrees are
    recursively affected, we have to use the following recursive definition:

    1. No tree has depth smaller than its `avgDepth`.
    2. Left subtree of a binary parent node is generated with `avgDepth :=
       (parentAvgDepth - 1) * avgDepthDecay`, where `avgDepthDecay` is a real
       number between 0 and 1. Smaller values of the decay cause greater unbalance
       of depths of left and right subtrees because they allow more shallow
       left subtrees and right subtrees then must be deeper, for the parent to
       satisfy the minimum average depth of its subtrees.
    3. Right subtree of a binary parent node is then generated after its left
       sibling (this way `leftSiblingDepth` is known) with `avgDepth := 2 *
       (parentAvgDepth - 1) - leftSiblingDepth`.
    4. The single subtree of an unary parent node has `avgDepth :=
       parentAvgDepth - 1`.

### CLI

The program generates multiple LTL formulae. Each of the generations can have
slightly different properties (to be able to generate very diversiform
formulae). The first four arguments set the four properties of the generator
of the first formula. Then, each other formula can increase these parameters
by a step.

There are 11 positional arguments (yeah, the CLI is not for humans):

  - `terminalCount[1]` - (greater than 0) sets the `terminalCount` property of the
    first formula, e.g. 3
  - `depth[1]` - (greater than 0) sets the `depth` property of the first formula, e.g. 5
  - `avgDepthRatio[1]` - (between 0 and 1) `avgDepth` of the first formula is
    `depth * avgDepthRatio`, e.g. 0.8
  - `avgDepthDecay[1]` - (between 0 and 1) sets the `avgDepthDecay` property of
    the first formula, e.g. 0.9
  - `terminalCountStep` - usually 0 or a tiny number, e.g. 0.005
  - `depthStep` - usually 0 or some small number, e.g. 0.6
  - `avgDepthRatioStep` - usually 0 or a tiny number, e.g. 0
  - `avgDepthDecayStep` - usually 0 or a tiny number, e.g. 0
  - `stepStep` - usually slightly above 1, e.g. 1.00004
  - `count` - how many big formulae we want, e.g. 3000
  - `growingCount` - how many growing formulae we want, e.g. 10000

There will be `growingCount` formulae generated with growing properties and then
`count` formulae generated with constant properties - the properties of the
last growing formula.

The growth is logarithmic, as the size of the formula grows usually
exponentially with the value of the properties. But then, in big numbers it is
exponential. Don't ask me why exactly (probably we used the logarithmic and the
~linearly growing part of the curve) - we were experimentally trying to make
approximately linear growth of the formula size and this worked.

Growing formulae are indexed by `i`. Then,
`logi[i] = stepStep^(i - 1) * log_2(i)`

The `logi` is the log-lin-exp growing curve. All the parameters are simply comupted as
`terminalCount[i] = terminalCount[1] + terminalCountStep * logi`
...

The `terminalCount` and `depth` properties are integers, so they get rounded.

## ltl-generator-exe

Generate systematic and parametric benchmarks. The second argument is the count
of the generated LTL formulae (terminated by newline). There are four possible options
for the first argument:

- `lift` - the standard lift benchmark, each floor has its own binary variable
- `liftBin` - the standard lift benchmark, floors are encoded as binary numbers -
  the number of variables is logarithmic wrt floor count
- `counter` - the standard counter benchmark
- `<integer n>` - systematically generate all LTL formulae (start with the
  smallest one) using operators `& ! X G`, and `n` variables (`a0 a1 ...
  a<n-1>`). Operators with structurally identical operands are skipped (e.g. `a0 & a0`).
  Similarly, commutativity and idempotence of `&` is taken into account and `a0
  & a1` is included into the sequence while `a0 & a0` and `a1 & a0` are
  excluded. If n=2, the sequence starts with:
  ```
  a0
  a1
  (& a0 a1)
  (! a0)
  (! a1)
  (X a0)
  (X a1)
  (G a0)
  (G a1)
  (& (& a0 a1) (! a0))
  ```
  These formulae might be useful for debugging, when one wants to find the
  smallest LTL that violates his expectation. Other operators are currently
  commented out in the code and can be easily added.

## ltl-translators-exe

- parse one of the supported LTL formats (pltl, spin, spot, pltl, prefix ltl)
- print it in the prefix ltl format (as always)
