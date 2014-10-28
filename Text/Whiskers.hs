{-# LANGUAGE TemplateHaskell #-}

module Text.Whiskers (whiskers, whiskersFile) where

import Control.Applicative ((<*>))
import Data.List
import Data.Maybe
import Data.String
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.ParserCombinators.Parsec

data Token = Str String | Var String deriving (Show)

whiskers :: QuasiQuoter
whiskers = QuasiQuoter { quoteExp  = parseWhiskers
                       , quotePat  = undefined
                       , quoteType = undefined
                       , quoteDec  = undefined
                       }

whiskersFile :: QuasiQuoter
whiskersFile = QuasiQuoter { quoteExp  = \s -> runIO (readFile s) >>= parseWhiskers
                           , quotePat  = undefined
                           , quoteType = undefined
                           , quoteDec  = undefined
                           }

parseWhiskers :: String -> ExpQ
parseWhiskers s = either (error . show) buildExp (parse parser "(unknown)" s)

buildExp :: [Token] -> ExpQ
buildExp tokens = [| fromString (concat $chunks) |]
  where
    vars = nub [x | Var x <- tokens]
    env = [(x, findName x) | x <- vars]
    chunks = fmap ListE $ sequence $ map (tokenExp env) tokens 

findName :: String -> Q Name
findName s = lookupValueName s >>= maybe err return 
  where err = error ("unknown variable: " ++ s)

tokenExp :: [(String, Q Name)] -> Token -> Q Exp
tokenExp _   (Str x) = return . LitE $ StringL x
tokenExp env (Var x) = fmap VarE $ fromJust (lookup x env)

parser :: Parser [Token]
parser = many1 (str <|> var <|> brackets)

str :: Parser Token
str = fmap Str $ many1 (noneOf "{")
    
var :: Parser Token
var = try $ do
    string "{{"
    spaces
    x <- letter
    xs <- many alphaNum
    spaces
    string "}}"
    return $ Var (x:xs)

brackets :: Parser Token
brackets = do
    x <- char '{'
    y <- try (noneOf "{")
    return $ Str (x:[y])
