# Lambda calculus &amp; proof theory

A proof-of-concept implementation of the most fundamental concepts of lambda calculus:
- nameless lambda terms with DeBruijn indexing (compressed representation)
- lambda term parser (into intermediate named lambda terms, converted to nameless terms)
- beta-reduction
- type inference (Algorithm W) (!)
- binary encoding/decoding
- Church numerals, booleans & general practical uses

## Example usage:
```Haskell
> :load lcpt.hs
```
The LCPT module serves as to unify the exported functionality of the other modules.
It is recommended to import LCPT, rather than any of the others.

### Parsing lambda terms
The parsing function returns an optional compressed nameless lambda term.
```Haskell
> :t ps
ps :: String -> Maybe Lambda
> let (Just t) = ps "lambda[x,y,z]xz(yz)"
> t
lambda[u,v,w]uw(vw)
```
The parsed format resembles closely the format used by humans - variables can only be named one of `x,y,z,u,v,w` (perhaps followed by a number), abstraction can bind multiple variables (`"lambda[x,y].."` can be used instead of `"lambda[x]lambda[y]"`) and application of terms is done by simply concatenating them, without any other syntactic constructs. Of course, brackets can be placed at will and will be taken into account.

### Nameless lambda terms - implementation details
The nameless lambda terms are _compressed_, meaning:
- repeated abstraction `lambda[x]lambda[y]..` is internally represented as one node in the tree, with the number of successively bound variables
- function application on a term on multiple arguments is also represented as one node, containing a list of terms (constructed with the `Ap` constructor). The tail of this list are the arguments, applied to the head, f.e. `Ap [s,k,k]` is equivalent to `((s k) k)`
- standard DeBruijn indexing is used, meaning alpha-equivalence comes free as pure syntactic equivalence

### Beta reduction
```Haskell
> :t beta
beta :: Lambda -> Lambda
> let t2 = Ap [s,k,k]
> beta t2
λ[u]u
> i == beta t2
True
> w == beta w
True
```
Some standard combinators are exposed for convenience:
- the identity function I = `λ[u]u` (exported as `i`)
- the constant functions K = `λ[u,v]u` and K* = `λ[u,v]v` (`k` and `ks`)
- the substitution operator S = `λ[u,v,w]uw(vw)` (`s`)
- the irreducible terms ω = `λ[u]uu` and Ω = ωω = `(λ[u]uu)(λ[u]uu)` (exported as `w` and `om`)
- the fixed-point combinator Y = `λ[u](λ[v]u(vv))(λ[v]u(vv))` (`y`)
- Chris Barker's iota combinator J (`j`) = `L 1 (Ap [Var 0, s, k])`:
```Haskell
> i == beta (Ap [j,j])
True
> ks == beta (Ap [j, Ap[j,j]])
True
> k == beta (Ap [j, Ap [j, Ap [j,j]]])
True
> s == beta (Ap [j, Ap [j, Ap [j, Ap [j,j]]]])
True
```
### Beta reduction in details
- the function `betaStep` performs one step of beta-reduction, if possible
- `beta` performs a full reduction, but for ease-of-use acts as the identity on irreducibles
- `betaSteps` gives a list of all intermediate steps
- `betaSteps_` prints all intermediate steps

### Type inference
An implementation of the classic Algorithm W for type inference, adapted for compressed nameless lambda terms from [this repo](https://github.com/wh5a/Algorithm-W-Step-By-Step).

As expected, beta reduction preserves the type. The function `infer` does the heavy lifting, whereas `infer_` simply pretty-prints the result. Upon failure, a detailed error message is printed.
```Haskell
> :t infer
infer :: Lambda -> Either String Type
> :t infer_
infer_ :: Lambda -> IO ()
> infer_ i
λ[u]u :: a -> a

> infer_ s
λ[u,v,w]uw(vw) :: (a -> b -> c) -> (a -> b) -> a -> c

> let t = Ap [s,k,k]
> infer_ t
(λ[u,v,w]uw(vw))(λ[u,v]u)(λ[u,v]u) :: a -> a

> mapM_ infer_ (betaSteps t)
(λ[u,v,w]uw(vw))(λ[u,v]u)(λ[u,v]u) :: a -> a

(λ[u,v](λ[w,x]w)v(uv))(λ[u,v]u) :: a -> a

λ[u](λ[v,w]v)u((λ[v,w]v)u) :: a -> a

λ[u](λ[v]u)((λ[v,w]v)u) :: a -> a

λ[u]u :: a -> a

> infer_ w
λ[u]uu
  occurs check fails: a vs. a -> b
  in uu
  in λ[u]uu
```

### Binary encoding/decoding
\[WIP]

### Church numerals, booleans & arithmetic
\[WIP]
