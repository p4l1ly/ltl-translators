module Main where

import System.Environment

import Ltl
import qualified ParseDummy
import qualified ParseLtlf
import qualified ParsePltl
import qualified ParseSpin
import qualified ParseSpot

main :: IO ()
main = do
  (fmt : other) <- getArgs
  let parseLtl = case fmt of
        "pltl" -> ParsePltl.parseLtl
        "spot" -> ParseSpot.parseLtl
        "spin" -> ParseSpin.parseLtl
        "dummy" -> ParseDummy.parseLtl
        "ltlf" -> ParseLtlf.parseLtl

      show'
        | "out=spin" `elem` other = showSpin
        | otherwise = show

  interact $ show' . flatten . svarsToVars . parseLtl
