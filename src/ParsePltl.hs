{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}

module ParsePltl (
  parseLtl,
) where

import Data.Char
import Data.Either
import Text.InterpolatedString.Perl6 (qq)
import Text.Parsec
import Text.Parsec.Expr

import Ltl
import ParseTokens

idParser :: WParser [String]
idParser = many $ tsatisfy $ const True

parseLtl :: String -> Ltl
parseLtl str = case runWParser expr str of
  Right x -> x
  Left err -> error $ show err

parens :: WParser a -> WParser a
parens = between (tok "(") (tok ")")

term :: WParser Ltl
term =
  parens expr
    <|> (Not <$> (tok "~" *> term))
    <|> (Next <$> (tok "X" *> term))
    <|> (Until <$> (tok "U" *> term) <*> term)
    <|> (WeakUntil <$> (tok "W" *> term) <*> term)
    <|> (Globally <$> (tok "G" *> term))
    <|> (Finally <$> (tok "F" *> term))
    <|> (Release <$> (tok "R" *> term) <*> term)
    <|> (LTrue <$ tok "1")
    <|> (LFalse <$ tok "0")
    <|> (SVar <$> tsatisfy (all isAlphaNum))

expr = buildExpressionParser table term

table =
  [ [Infix ((\x y -> And [x, y]) <$ tok "&") AssocLeft]
  , [Infix ((\x y -> Or [x, y]) <$ tok "|") AssocLeft]
  , [Infix ((\x y -> Or [Not x, y]) <$ tok "=>") AssocLeft]
  , [Infix ((\x y -> Or [And [x, y], And [Not x, Not y]]) <$ tok "<=>") AssocLeft]
  , [Infix ((\x y -> Until x y) <$ tok "U") AssocLeft]
  , [Infix ((\x y -> WeakUntil x y) <$ tok "W") AssocLeft]
  ]
