{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Bits hiding (And)
import Data.List
import System.Environment
import Math.NumberTheory.Logarithms
import System.IO

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
        , And$
            zipWith3 (\a b c -> b -=> Next (Or [a, b, c]))
              fs (tail fs) (tail (tail fs))
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
    , And$ flip map fs$ \f ->
        And [f, Not sb] -=> Until f (sb `Release` And [Finally f, Not up])
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


generateCounter :: Int -> Ltl
generateCounter n = And
  [ m, And$ map (\i -> nexts i (Not b)) [0..n-1]
  , Globally$ And
    [ m -=> And (nexts n m : map (\i -> nexts i (Not m)) [1..n-1])
    , And [m, Not b] -=> And [Not c, nexts n b]
    , And [m, b] -=> And [c, nexts n (Not b)]
    , And [Not c, Next (Not m)] -=> And
        [ Next (Not c)
        , Next b -=> nexts (n+1) b
        , Next (Not b) -=> nexts (n+1) (Not b)
        ]
    , c -=> And
        [ Next (Not b) -=> And [Next (Not c), nexts (n+1) b]
        , Next b -=> And [Next c, nexts (n+1) (Not b)]
        ]
    ]
  ]
  where
  m = Var 0
  b = Var 1
  c = Var 2
  nexts count x = iterate Next x !! count


main = do
  [terminalCnt, cnt] <- getArgs
  let ltls = take (read cnt)$
        case terminalCnt of
          "lift" -> map (generateLift False) [3..]
          "liftBin" -> map (generateLift True) [3..]
          "counter" -> map generateCounter [3..]
          x -> generate$ read terminalCnt
  flip mapM (zip [0..] ltls)$ \(i, ltl) -> do
    hPrint stderr i
    print ltl
