module FunctionWithWhere where

printInc n = print plusTwo
  where plusTwo = n + 2

mult1 = x * 3 + y
  where x = 3
        y = 1000

mult2 = x * 5
  where x = 10 * 5 + y
        y = 10

div1 = z / x + y
  where x = 7
        y = negate x
        z = y * 10