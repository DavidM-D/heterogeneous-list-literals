# Heterogeneous List Literals

This is an incredibly simple library, which makes writing literals for heterogeneous collections easy

If you write a function with the signature
```haskell
hList :: HLL input output => input -> HList output
```
then
```haskell
a :: HList '[]
a = hList ()

b :: HList '[Bool]
b = hList (OneTuple True)

c :: HList '[Bool, Int, Double, String]
c = hList (True, 24, 10.5, "Fire")
```
The full code is in test/Docs.hs

This only supports literals of length up to 20, though that can be easily extended using the code generator in src/Data/HeterogeneousListLiterals.hs
