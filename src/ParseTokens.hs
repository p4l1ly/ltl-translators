module ParseTokens
  ( tok
  , tsatisfy
  , runWParser
  , WParser
  ) where

import Control.Monad
import Data.Char
import Text.Parsec hiding (tokens)
import Util

tokens :: Parsec String () [String]
tokens = concat <$> (spaces *> tokensNoSpaces `sepEndBy` spaces)

tokensNoSpaces :: Parsec String () [String]
tokensNoSpaces = many1 ((singleton <$> oneOf "(){}") <|> many1 alphaNum <|> many1 notAlphaNum)
  where notAlphaNum = satisfy $ \x ->
          not (isAlphaNum x) && not (isSpace x) && isPrint x && not (x `elem` "(){}")

tok :: [Char] -> WParser ()
tok t = void $ tsatisfy (== t) -- <?> [qq|token {t}|]

tsatisfy :: (String -> Bool) -> WParser String
tsatisfy f = tokenPrim id
  (\pos _ _ -> incSourceColumn pos 1)
  (\c -> if f c then Just c else Nothing)

type WParser a = Parsec [String] () a

runWParser :: WParser a -> String -> Either ParseError a
runWParser p input = runParser tokens () "" input >>= runParser p () ""
