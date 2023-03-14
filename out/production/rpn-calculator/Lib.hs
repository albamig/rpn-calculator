module Lib (calcRPN) where

calcRPN :: String -> Int
calcRPN = head . foldl rpnFold [] . words
  where
    rpnFold (x : y : ys) "*" = (x * y) : ys
    rpnFold (x : y : ys) "+" = (x + y) : ys
    rpnFold stack item = read item : stack
