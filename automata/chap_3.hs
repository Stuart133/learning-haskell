module Chap3 where 

root :: Float -> Float -> Float -> Float
root a b c = 
    ((-b) + sqrt (b ^ 2 - (4 * a * c))) / (2 * a)