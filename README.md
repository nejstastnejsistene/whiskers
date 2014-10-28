whiskers
========

Moustache templates with Template Haskell.

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Text.Whiskers

main :: IO ()
main = putStrLn [whiskers|
<!DOCTYPE html>
<html>
    <head>
        <title>Hello, {{ x }}!</title>
    </head>
    <body>
        <h1>This is {{ y }}.</h1>
    </body>
</html>
|] where x = "world"
         y = "whiskers"
 ```
