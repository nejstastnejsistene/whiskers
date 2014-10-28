{-# LANGUAGE QuasiQuotes #-}

import Whiskers

-- Equivalent to:
-- hello = \x -> concat ["Hello, ", x, "!"]
hello :: String -> String
hello = [whiskers|Hello, {{ x }}!|]

-- Prints "Hello, world!"
main :: IO ()
main = putStrLn (hello "world")
