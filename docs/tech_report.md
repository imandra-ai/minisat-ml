# Technical report on Minisat-ml

This is a technical report on an experiment we at [Imandra](https://imandra.ai)
have led on the comparative performance of C++, rust, and OCaml for writing
SAT solvers and similar computational logic programs. C++ is widely used
and can deliver very high performance, but unsafe and sometimes unwieldy.
The goal of this experiment is to assess the practicality of using OCaml
(or rust) to write satisfiability solvers.

Our methodology was to adapt [Minisat 2.2](http://minisat.se)
in OCaml ([minisat-ml](https://github.com/AestheticIntegration/minisat-ml))
and in rust ([batsat](https://github.com/c-cube/batsat/),
derived from [ratsat](https://github.com/qnighy/ratsat) which is
directly adapted from Minisat), and run them on sets of benchmarks to compare
the results. Batsat is inspired from Minisat but diverged somewhat due to
its parametrization over a theory, and some internal refactorings.

We also compare with [Z3 4.8.4](https://github.com/Z3Prover/z3/releases/tag/z3-4.8.4)
and [msat](https://github.com/Gbury/mSAT/). Z3 is an alternative SMT and SAT
solver in C++. Msat is an OCaml SAT solver that is roughly inspired from Minisat
but has significant implementations differences, is functorized over a theory,
and doesn't have the same heuristics parameters.
As an OCaml codebase, its style is more idiomatic and generally, more readable
than Minisat-ml.

## Implementation of Minisat-ml

The code of Minisat-ml was written in a way that closely mirrors Minisat's
source. Some differences were unavoidable due to discrepancies between OCaml
and C++.

#### Modules

Instead of `.h` and `.cpp` files, the OCaml version uses the
module system with `.ml` and `.mli` files. The `heap` file was also modified
to be parametrized (using an OCaml functor) over its content, rather than
dealing entirely with integers.s
Inlining is still possible thanks to `[@@inline]` annotations on functors
and functions.

#### Imperative programming

OCaml doesn't support `return` nor `continue`/`break` in loops,
nor `do {} while()` loops.
We had to rely on exceptions to emulate the former, and tail-recursive
functions for the latter. Combined with the lack of (clean) overloading
of the `[]` operator, it makes the code clumsier.

Compare this fragment of conflict analysis in OCaml and C++:

```ocaml
while
  let v = Lit.var (Vec.get self.trail !index) in
  index := !index -1;
  not (seen self v)
do ()
done;

p := Vec.get self.trail (!index+1);
confl := reason_lit self !p;
set_seen self (Lit.var !p) false;
pathC := !pathC - 1;
```

```cpp
while (!seen[var(trail[index--])]);
p     = trail[index+1];
confl = reason(var(p));
seen[var(p)] = 0;
pathC--;
```

In C++, the custom `Vec` type (a resizable array that is used for
almost all data in Minisat) is manipulated like an array thanks to
`operator[]` overloading. In OCaml we use `Vec.get vec x` and `Vec.set vec x y`
to access and modify fields, which is more verbose.

#### Memory layout

OCaml doesn't provide value types, so we use explicit struct-of-arrays encoding
for watch lists (where the C++ version stores `struct Watcher(uint32,uint32)`
in a single vector, we have two `int Vec.t` of the same size to avoid allocating
lots of small records).

32 bits integers are replaced by 63 bits integers (on x86_64) since OCaml
only has word-sized values. Essentially, in Minisat-ml, almost everything is
a `int Vec.t` or a `foo Vec.t` where `type foo = private int` (e.g. variables
or literals or the ternary `Lbool.t`).

#### Optimizations

We use OCaml 4.06 + flambda, with the flags
`-O3 -unbox-closures -unbox-closures-factor 20`.
This allows a lot of function calls to be inlined, leading to large
performance gains.

Some functions are annotated as `[@@inline]` to guide the optimizer better.

## Discussion

The differences in performance can be explain by at least these factors:

- the write barrier, which isn't easy to elide since we use a polymorphic `'a Vec.t`
  for convenience. Specializing `Vec.t` on immediate/non-immediate types would
  certainly help here.
- more cache misses (since all data in OCaml is stored as 64-bits words on x86_64,
  whereas minisat uses more compact 32-bits integers everywhere)
- array bound checking (although we use `unsafe_get` and `unsafe_set` quite a lot)
- more arithmetic for accessing slices in the clause allocator: to get
  the `i`-th literal of a clause, we access `alloc.memory.(c + 1 + i)`.
  The clause `c` is an offset in the large integer array `alloc.memory`,
  and it points to the clause header, followed by literals.
  In contrast, the C++ version provides a local temporary struct with the header
  and a direct pointer to the first literal; subsequent accesses to literals are
  just `c.lits[i]`. Here rust shines by providing a bound-checked slice
  whose lifetime is properly limited, preventing the temporary clause struct
  from becoming stale.

The differences in behavior come from discrepancies in the VSIDS heuristics,
due to different floating point computations. At some point in non trivial
problems, variable activities in the C++ and OCaml versions diverge slightly
(after decay and renormalization, which involves a multiplication by `1e-20`),
which leads to a different variable being picked as a decision.
