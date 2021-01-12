{-# LANGUAGE
  QuasiQuotes,
  ExtendedDefaultRules,
  LambdaCase
#-}

module Ltl
  ( Ltl(..)
  , svarsToVars
  , flatten
  , showSpin
  ) where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Text.InterpolatedString.Perl6 (qq)

data Ltl
  = Var Int
  | SVar String
  | LTrue
  | LFalse
  | And [Ltl]
  | Or [Ltl]
  | Not Ltl
  | Next Ltl
  | Until Ltl Ltl
  | WeakUntil Ltl Ltl
  | Globally Ltl
  | Finally Ltl
  | Release Ltl Ltl

childs :: Ltl -> [Ltl]
childs (And xs) = xs
childs (Or xs) = xs
childs (Not x) = [x]
childs (Next x) = [x]
childs (Until x y) = [x, y]
childs (WeakUntil x y) = [x, y]
childs (Release x y) = [x, y]
childs (Globally x) = [x]
childs (Finally x) = [x]
childs _ = []

subnodes :: Ltl -> [Ltl]
subnodes x = x : concatMap subnodes (childs x)

transform :: (Ltl -> Maybe Ltl) -> Ltl -> Ltl
transform fn x = helper x
  where helper x = case fn x of
          Just x' -> x'
          Nothing -> case x of
            And xs -> And $ map helper xs
            Or xs -> Or $ map helper xs
            Not x -> Not $ helper x
            Next x -> Next $ helper x
            Until x y -> Until (helper x) (helper y)
            WeakUntil x y -> WeakUntil (helper x) (helper y)
            Release x y -> Release (helper x) (helper y)
            Globally x -> Globally $ helper x
            Finally x -> Finally $ helper x
            x -> x

svarsToVars :: Ltl -> Ltl
svarsToVars ltl = transform helper ltl
  where
    svars = S.toList $ S.fromList $ catMaybes $
      map (\case SVar x -> Just x; _ -> Nothing) $ subnodes ltl

    svarToVar = M.fromList $ zip svars [0..]

    helper (SVar x) = Just $ Var $ svarToVar M.! x
    helper _ = Nothing

instance Show Ltl where
  show (Var x) = [qq|a{x}|]
  show (SVar x) = [qq|"{x}"|]
  show LTrue = [qq|t|]
  show LFalse = [qq|f|]
  show (And xs) = [qq|(& {intercalate " " $ map show xs})|]
  show (Or xs) = [qq|(| {intercalate " " $ map show xs})|]
  show (Not x) = [qq|(! {x})|]
  show (Next x) = [qq|(X {x})|]
  show (Until x y) = [qq|(U {x} {y})|]
  show (WeakUntil x y) = [qq|(W {x} {y})|]
  show (Release x y) = [qq|(R {x} {y})|]
  show (Globally x) = [qq|(G {x})|]
  show (Finally x) = [qq|(F {x})|]

flatten :: Ltl -> Ltl
flatten = transform helper
  where
    helper (And xs) = Just $ And $ concatMap kidnap $ map flatten xs
      where kidnap = \case And xs -> xs; x -> [x]
    helper (Or xs) = Just $ Or $ concatMap kidnap $ map flatten xs
      where kidnap = \case Or xs -> xs; x -> [x]
    helper (Not (Not x)) = Just $ flatten x
    helper x = Nothing

showSpin (Var x) = [qq|a{x}|]
showSpin (SVar x) = [qq|"{x}"|]
showSpin LTrue = [qq|1|]
showSpin LFalse = [qq|0|]
showSpin (And xs) = [qq|({intercalate " && " $ map showSpin xs})|]
showSpin (Or xs) = [qq|(| {intercalate " || " $ map showSpin xs})|]
showSpin (Not x) = [qq|(! {showSpin x})|]
showSpin (Next x) = [qq|(X {showSpin x})|]
showSpin (Until x y) = [qq|({showSpin x} U {showSpin y})|]
showSpin (WeakUntil x y) = [qq|({showSpin x} W {showSpin y})|]
showSpin (Release x y) = [qq|({showSpin x} V {showSpin y})|]
showSpin (Globally x) = [qq|([] {showSpin x})|]
showSpin (Finally x) = [qq|(<> {showSpin x})|]
