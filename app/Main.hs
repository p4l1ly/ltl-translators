module Main where

import ParsePltl

main :: IO ()
main = interact $ show . parseLtl
