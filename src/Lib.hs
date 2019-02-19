{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Lib  where
import Language.Haskell.TH


someFunc :: IO ()
someFunc = putStrLn "someFunc"




data Expr = Val Int | Add Expr Expr

eval :: Expr -> TExpQ Int
eval (Val n) = [|| n ||]
eval (Add e1 e2) = [|| $$(eval e1) + $$(eval e2) ||]

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
type Code = TExpQ

-- unrolled fib woth sharing
fib 0 = 1
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

-- we always need [||  ||] wheneve there is a Code
fib' :: Int -> TExpQ Int
fib' 0 = [|| 1 ||]
fib' 1 = [|| 1 ||] 
fib' n = [|| $$(fib' (n-1)) + $$(fib' (n-2)) ||] -- 

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
