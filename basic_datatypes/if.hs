module If where

a = "AWESOME"
w = "wut"

ifFunc x = if (x + 1 == 1)
            then a
            else w

greetIfCool coolness =
  if cool
    then putStrLn "eyyyyy. What's shakin'?"
  else
    putStrLn "pshhh."
  where cool = coolness == "downright frosty yo"

greetIfCool2 coolness =
  if cool coolness
    then putStrLn "eyyyyy. What's shakin'?"
  else
    putStrLn "pshhh."
  where cool v =
            v == "downright frosty yo"
