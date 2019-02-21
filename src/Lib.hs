{-# LANGUAGE TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Lib  where
import Language.Haskell.TH


someFunc :: IO ()
someFunc = putStrLn "someFunc"




data Expr = Val Int | Add Expr Expr

eval :: Expr -> TExpQ Int
eval (Val n) = [|| n ||]
eval (Add e1 e2) = [|| $$(eval e1) + $$(eval e2) ||]

eval' :: Expr -> Int
eval' (Val n) = n
eval' (Add e1 e2) = (eval' e1) + (eval' e2)

-- can one derive eval' from eval? eval from eval'?
-- Therein lies the questions


eval'' :: (Applicative m) => Expr -> m Int
eval'' (Val n) = pure n
eval'' (Add e1 e2) = (+) <$> (eval'' e1) <*> (eval'' e2)

-- eval' = runIdentity . eval''
-- eval = ? Is TExpr an acceptable applicative like this?
-- I kind of doubt it. The [|| ||] and $$ forms are more magical than that
-- maybe this is why the finally tagless indirection is necessary?

newtype Code a = Code (TExpQ a)
class ExprS repr where
    val :: Int -> repr Int
    add :: repr (Int -> Int -> Int) -- repr Int -> repr Int -> repr Int ? Why not this?
instance ExprS Code where
    val n = Code $ [|| n ||]
    add =  Code $  [|| \x y -> x + y ||]

power 0 x = 1
power n x = x * (power (n-1) x)


power' :: Int -> TExpQ (Int -> Int)
power' 0 = [|| const 1 ||]
power' n = [|| \x -> x * $$(power' (n-1)) x ||] 

{-
data HOAS a where
    Lam :: a -> HOAS b -> HOAS (a -> b)
    App :: HOAS (a -> b) -> HOAS a -> HOAS b
    Lit :: a -> HOAS a

-- this is exactly Applicative. Minus the laws?
class SSym repr where
    lit :: a -> repr a
    app :: repr (a -> b) -> repr a -> repr b

instance SSym HOAS where
    lit = Lit
    app = App

class SLam repr where
    lam :: repr a -> repr b -> repr (a -> b) 
-}


-- unrolled fib woth sharing
fib 0 = 1
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

-- we always need [||  ||] wheneve there is a Code
fib' :: Int -> TExpQ Int
fib' 0 = [|| 1 ||]
fib' 1 = [|| 1 ||] 
fib' n = [|| $$(fib' (n-1)) + $$(fib' (n-2)) ||] -- 
{-
sfib x y 0 _ = y  
sfib x y 1 _ = x
sfib x y n = sfib y (x+y) (n-1)
-}
-- from the haskell wiki

fib3 n = go n ( [|| 0 ||], [|| 1 ||] )
  where
    go n (a, b) | n==0      = a
                | otherwise = go (n-1) (b, [|| $$(a) + $$(b) ||] )
fib4 :: Int -> TExpQ Int
fib4 n = go n [|| ( 0, 1 ) ||]
                where
                  go :: Int -> TExpQ (Int, Int) -> TExpQ Int
                  go n z | n==0      = [|| let (a,b) = $$(z) in a ||]
                         | otherwise = go (n-1) [|| let (a,b) = $$(z) in (b, a + b) ||]

-- sharing?
fib'' :: Int -> TExpQ Int
fib'' 0 = [|| 1 ||]
fib'' 1 = [|| 1 ||] 
fib'' n = [|| $$(fib' (n-1)) + $$(fib' (n-2)) ||] -- 

-- I mean a part of doing this with templates, is how would you write it by hand?

ex3 :: String
ex3 = $$( [|| "fred" ||] )

-- a category of template haskell?


-- .< >. metaocaml
-- [||   ||] type templatre haskell

-- escape .~
-- $$ splice. turns TExpQ a into a 

{-
data Expr = Lit Int | Add Expr Expr | Times Expr Expr

eval :: Expr -> Code Int
eval (Lit n) = [|| n  ||]
eval (Add x y) = [|| $$(eval x) + $$(eval y)  ||]
eval (Mul x y) = [|| $$(eval x) * $$(eval y)   ||]
-}
-- what about adding binding forms?