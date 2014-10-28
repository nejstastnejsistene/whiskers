whiskers
========

Moustache templates with Template Haskell.

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Text.Whiskers

-- Equivalent to:
-- hello = \x -> concat ["Hello, ", x, "!"]
hello :: String -> String
hello = [whiskers|Hello, {{ x }}!|]

-- Prints "Hello, world!"
main :: IO ()
main = putStrLn (hello "world") -- Prints "Hello, world!"
```
