# staged

A repo for playing with staged metaprogramming in Haskell.

Links:


https://www.youtube.com/watch?v=AzJVFkm42zM&t=1995s

http://mpickering.github.io/posts/2019-02-14-stage-3.html

https://github.com/mpickering/three-level

http://okmij.org/ftp/meta-programming/index.html

https://www.nowpublishers.com/article/Details/PGL-038

https://markkarpov.com/tutorial/th.html

quotations
splice - turns 

lifting - cross stage persistance thorugh serialization?

typed template haskell as of ghc 7.8. Two levels only? no polymorphic lift. 

Not necessarily connected, but often occur together - tagless final
tagless final makes a dsl by replacing every constructor of your dsl data type by a method in a typeclass
data FreeNum a = Add FreeNum FreeNum | Sub FreeNum | Pure a | Times FreeNum FreeNum | 
data FreeMonoid a = MAppend FreeMonoid FreeMonoid | Mempty | Pure a

->

class Num
class Monoid
etc.


sometimes your dsl includes binding forms, in which case everything gets a little more confusing.



The template haskell dump is in the .stack-work folder deep down
dist/x86/Cabal/build/ src or exectutable in .dump-splices files