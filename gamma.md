### Goal: define an operator Г, whose least fixed point is the set of the Fibonacci numbers.

Let Г(X) = {0,1,2} ∪ { x+y | x,y∈X, x>y, x≤2y }.

First we'll show that Г is monotonous. Let X⊆Y and k∈Г(X). If k∈{0,1,2}, then k∈Г(Y) for every Y. Otherwise, if k=x+y for some x,y∈X, such that x>y and x≤2y, then x,y∈Y and k=x+y∈Г(Y). Therefore Г(X)⊆Г(Y). It should be obvious that if k∈{0,1,2}, then k∈Г(X) for every set X.

Next we need to prove that Г is a continuous operator: for each sequence X<sub>0</sub>⊆X<sub>1</sub>⊆X<sub>2</sub>⊆...⊆X<sub>n</sub> it holds that ∪Г(X<sub>i</sub>) = Г(∪X<sub>i</sub>) (for every i∈{0,1,...})
- ∪Г(X<sub>i</sub>)⊆Г(∪X<sub>i</sub>). Let k∈∪Г(X<sub>i</sub>). Again, if k∈{0,1,2}, then k∈Г(X<sub>i</sub>) for every i ⇒ k∈Г(∪X<sub>i</sub>) (here we use that X<sub>i</sub>⊆∪X<sub>i</sub>). Otherwise, if k=x+y, then k∈Г(X<sub>i</sub>) for some i (this is an enumerable sequence), meaning x,y∈X<sub>i</sub> ⇒ x,y∈∪X<sub>i</sub> ⇒ k∈Г(∪X<sub>i</sub>).
- Г(∪X<sub>i</sub>)⊆∪Г(X<sub>i</sub>). Let k∈(∪X<sub>i</sub>). If k=x+y as described above, then x,y∈∪X<sub>i</sub> ⇒ x∈X<sub>i</sub> and y∈X<sub>j</sub> for some i,j≥0. We can assume i≥j ⇒ X<sub>j</sub>⊆X<sub>i</sub> ⇒ x,y∈X<sub>i</sub> ⇒ k∈Г(X<sub>i</sub>) ⇒ k∈∪Г(X<sub>i</sub>).

From these two proofs it follows that the least fixed point of Г is ∪Г<sup>n</sup>(∅). We can observe:
- Г(∅) = {0,1,2}
- Г(Г(∅)) = {0,1,2,3}
- Г(Г(Г(∅))) = {0,1,2,3,5}
- Г(Г(Г(Г(∅)))) = {0,1,2,3,5,8}

Let X<sub>0</sub> = ∅ and X<sub>i+1</sub> = Г(X<sub>i</sub>) for each i≥0. We will prove by induction that each application of Г adds only the next Fibonacci number to the set, meaning ∪Г<sup>n</sup>(∅) is the set of all Fibonacci numbers. More formally: 
- Let F<sub>i</sub> be the i-th Fibonacci number, where F<sub>0</sub>=0, F<sub>1</sub>=1 and F<sub>i+2</sub>=F<sub>i+1</sub>+F<sub>i</sub> for each i≥0. Then Г<sup>n</sup>(∅) = {F<sub>i</sub> | i∈{0,...,n+2}} for every n≥1.

For n=1 we have Г(∅) = {0,1,2} and the statement is true. Now let the statement be true for a given n≥1. We will prove it for n+1. The way Г works is, for any given x in X, take every y∈[x/2;x) and add it to x, to obtain Г(X). However, for the Fibonacci sequence we know that F<sub>i+1</sub>/F<sub>i</sub>=2 for i=2 and decreases with each following i, with the limit being φ≈1.618 as n increases infinitely. Alternatively, we have 0.5≤F<sub>i</sub>/F<sub>i+1</sub>≤0.618 ⇒ F<sub>i</sub>/F<sub>i+2</sub>≤0.38, etc. Therefore, for every F<sub>i</sub> there is exactly one other Fibonacci number in [F<sub>i</sub>/2;F<sub>i</sub>) and that number is F<sub>i-1</sub>. This means that Г(Г<sup>n</sup>(∅)) takes every number F<sub>i</sub>∈Г<sup>n</sup>(∅) = {F<sub>0</sub>,...,F<sub>n+2</sub>} (as per inductive hypothesis) and adds to it F<sub>i-1</sub> ⇒ Г<sup>n+1</sup>(∅) = {F<sub>i</sub> | i∈{0,...,n+3}}, as required.

Addendum: a Haskell implementation of Г.
```Haskell
import Data.List (sort, nub)

gamma :: [Int] -> [Int]
gamma l = makeSet $ [0,1,2] ++ [ x+y | x<-l, y<-l, x>y, x<=2*y ]
  where makeSet = sort . nub
```