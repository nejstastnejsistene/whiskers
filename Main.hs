{-# LANGUAGE QuasiQuotes #-}

import Text.Whiskers

main :: IO ()
main = putStrLn [whiskersFile|README.md|]
  where
    x = "hello"
    y = "world"
