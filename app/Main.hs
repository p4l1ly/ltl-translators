module Main where

import Ltl
import ParsePltl

main :: IO ()
main = interact $ show . flatten . svarsToVars . parseLtl
