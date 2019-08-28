module ParseDummy
  ( parseLtl
  ) where

import Data.Char
import Control.Monad
import Data.Either
import Text.Parsec

import Ltl

type WParser a = Parsec String () a
runWParser p = runParser p () "" . filter (not . isSpace)

parseLtl :: String -> Ltl
parseLtl str = case runWParser term str of
  Right x -> x
  Left err -> error $ show err

parens :: WParser a -> WParser a
parens = between (char '(') (char ')')

operator :: WParser Ltl
operator = (And <$> (char '&' *> many1 term))
       <|> (Or  <$> (char '|' *> many1 term))
       <|> (Not <$> (char '!' *> term))
       <|> (Next <$> (char 'X' *> term))
       <|> (Until <$> (char 'U' *> term) <*> term)
       <|> (WeakUntil <$> (char 'W' *> term) <*> term)
       <|> (Globally <$> (char 'G' *> term))
       <|> (Finally <$> (char 'F' *> term))
       <|> (Release <$> (char 'R' *> term) <*> term)

term :: WParser Ltl
term = parens operator
   <|> (Var . read <$> (char 'a' *> many1 digit))
   <|> (LTrue <$ char 't')
   <|> (LFalse <$ char 'f')
