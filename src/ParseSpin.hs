{-# LANGUAGE
  QuasiQuotes,
  ExtendedDefaultRules
#-}


module ParseSpin
  ( parseLtl
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
term = parens expr
   <|> (Not <$> (tok "!" *> term))
   <|> (Next <$> (tok "X" *> term))
   <|> (Globally <$> (tok "[]" *> term))
   <|> (Finally <$> (tok "<>" *> term))
   <|> (LTrue <$ tok "1")
   <|> (LFalse <$ tok "0")
   <|> (SVar <$> tsatisfy (all isAlphaNum))

expr = buildExpressionParser table term

linfix x = Infix x AssocLeft

table = [ [ linfix $ Until <$ tok "U"
          , linfix $ WeakUntil <$ tok "W"
          , linfix $ Release <$ tok "V"
          ]
        , [ linfix $ (\x y -> And [x, y]) <$ (tok "&&" <|> tok "/\\") ]
        , [ linfix $ (\x y -> Or [x, y]) <$ (tok "||" <|> tok "\\/") ]
        , [ linfix $ (\x y -> Or [Not x, y]) <$ tok "->"
          , linfix $ (\x y -> Or [And [Not x, Not y], And [x, y]]) <$ tok "<->"
          ]
        ]
