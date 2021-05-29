{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Bits
import Data.List
import System.Environment
import Math.NumberTheory.Logarithms

import Safe (initMay)

import Ltl (Ltl(..), (<=>), (-=>))

generate :: Int -> [Ltl]
generate terminalCnt = concat result
  where
    terminals = map Var [0..terminalCnt - 1]

    result = map upToDepth [0..]

    upToDepth 0 = terminals
    upToDepth i = concat
      [ map And comb2
      -- , map Or combinations2
      , map Not greatest
      , map Next greatest
      -- , variations2 Until
      -- , variations2 WeakUntil
      -- , variations2 Release
      , map Globally greatest
      -- , map Finally childs
      ]
      where smaller = concat$ take (i - 1) result
            greatest = result !! (i - 1)

            comb2 = combGreatest ++ combSmaller

            combGreatest = case initMay (tails greatest) >>= initMay of
              Just tails_ -> [[x, y] | x:ys <- tails_, y <- ys]
              _ -> []
            combSmaller = [[x, y] | x <- greatest, y <- smaller]

            -- variations2 fn =
            --   [fn x y | x <- greatest, y <- smaller] ++
            --   [fn y x | x <- greatest, y <- smaller] ++
            --   [fn x y | x <- greatest, y <- greatest]


generateLift :: Bool -> Int -> Ltl
generateLift binary n = And
  [ Not u, head fs, And$ map Not bs, Not up
  , Globally$ And
    [ if binary then LTrue else And$
        map (\(f:hfs) -> f -=> And (map Not hfs))$ init$ tails fs
    , u <=> Not (Next u)
    , And
        [ u -=> And (map (\x -> x <=> Next x) fs)
        , f0 -=> Next (Or [f0, f1])
        , And$ zipWith3 (\a b c -> b -=> Next (Or [a, b, c])) fs (tail fs) (tail (tail fs))
        , fl -=> Next (Or [fll, fl])
        ]
    , And
        [ Not u -=> And (map (\x -> x <=> Next x) bs)
        , And$ zipWith (\b f -> And [b, Not f] -=> Next b) bs fs
        ]
    , And
        [ And$ map (\f -> And [f, Next f] -=> (up <=> Next up)) fs
        , And$ zipWith (\fa fb -> And [fa, Next fb] -=> up) fs (tail fs)
        , And$ zipWith (\fa fb -> And [Next fa, fb] -=> Not up) fs (tail fs)
        ]
    , sb <=> Or bs
    , And$
        map (\f -> And [f, Not sb] -=> Until f (Release sb (And [Finally f, Not up]))) fs
    , And$ zipWith (\b f -> b -=> Finally f) bs fs
    ]
  ]
  where
  fl = last fs
  fll = last$ init fs
  fs@(f0:f1:_)
    | binary = flip map [0..n-1]$ \f -> And$ flip map [0..bitCount - 1]$ \b ->
        if testBit f b then Var (b+3) else Not (Var (b+3))
    | otherwise = map Var [n+3..n+3+n-1]
  bitCount
    | 2^bc == n = bc
    | otherwise = bc + 1
    where bc = integerLog2$ fromIntegral n
  bs = map Var [0..n-1]
  up = Var n
  sb = Var$ n+1
  u = Var$ n+2


main = do
  [terminalCnt, cnt] <- getArgs
  mapM print$ take (read cnt)$
    case terminalCnt of
      "lift" -> map (generateLift False) [3..]
      "liftBin" -> map (generateLift True) [3..]
      x -> generate$ read terminalCnt
