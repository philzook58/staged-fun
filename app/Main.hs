{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-} 
module Main where

import Lib

main :: IO ()
main = someFunc

ex1 :: Int
ex1 = $$(eval (Add (Val 1) (Val 1)))

ex2 :: Int -> Int
ex2 = $$(power' 3)

ex4 = $$(fib' 5)