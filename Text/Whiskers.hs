{-# LANGUAGE TemplateHaskell #-}

module Text.Whiskers (whiskers) where

import Data.List
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

parseWhiskers :: String -> ExpQ
parseWhiskers s = either (error . show) buildExp (parse parser "(unknown)" s)

buildExp :: [Token] -> ExpQ
buildExp tokens = fmap lambda [| concat $(return chunks) |]
  where
    vars = nub [x | Var x <- tokens]
    env = [(x, mkName x) | x <- vars]
    lambda = LamE $ map (VarP . snd) env
    chunks = ListE $ map (tokenExp env) tokens

tokenExp :: [(String, Name)] -> Token -> Exp
tokenExp _   (Str x) = LitE $ StringL x
tokenExp env (Var x) = VarE $ maybe (error "var not found") id (lookup x env)

parser :: Parser [Token]
parser = many1 (str <|> var)

str :: Parser Token
str = fmap Str $ many1 (noneOf "{")
    
var :: Parser Token
var = do
    char '{'
    char '{'
    spaces
    x <- letter
    xs <- many alphaNum
    spaces
    char '}'
    char '}'
    return $ Var (x:xs)
