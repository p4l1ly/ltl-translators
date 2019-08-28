{-# LANGUAGE
  QuasiQuotes,
  ExtendedDefaultRules
#-}


module ParseSpot
  ( parseLtl
  ) where

import Data.Char
import Data.Either
import Text.InterpolatedString.Perl6 (qq)
import Text.Parsec
import Text.Parsec.Expr
import Util

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
term = parens expr
   <|> (Not <$> (tok "!" *> term))
   <|> (Next <$> (tok "X" *> term))
   <|> (Globally <$> (tok "G" *> term))
   <|> (Finally <$> (tok "F" *> term))
   <|> (LTrue <$ tok "1")
   <|> (LFalse <$ tok "0")
   <|> (SVar <$> tsatisfy (all isAlphaNum))

expr = buildExpressionParser table term

linfix x = Infix x AssocLeft

table = [ [ linfix $ (\x y -> Until x y) <$ tok "U"
          , linfix $ (\x y -> WeakUntil x y) <$ tok "W"
          , linfix $ (\x y -> Release x y) <$ tok "R"
          ]
        , [ linfix $ (\x y -> And [x, y]) <$ tok "&&"
          , linfix $ (\x y -> And [x, y]) <$ tok "&"
          ]
        , [ linfix $ (\x y -> Or [x, y]) <$ tok "||"
          , linfix $ (\x y -> Or [x, y]) <$ tok "|"
          ]
        , [ linfix $ (\x y -> Or [Not x, y]) <$ tok "->" ]
        ]
