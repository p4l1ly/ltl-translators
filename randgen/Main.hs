{-# LANGUAGE ViewPatterns, RecordWildCards, NamedFieldPuns, TupleSections #-}
module Main where

import Control.Arrow
import Control.Monad.State
import Data.List
import Data.Maybe
import System.Environment
import System.Random

import Ltl (Ltl(..))

weightsAll =
  [ (mkAnd, 1.0)
  , (mkNot, 1.0)
  , (mkNext, 1.0)
  , (mkUntil, 1.0)
  , (mkVar, 3.0)
  ]

weightsTerm =
  [ (mkVar, 1.0)
  ]

weightsNonTerm =
  [ (mkAnd, 1.0)
  , (mkNot, 1.0)
  , (mkNext, 1.0)
  , (mkUntil, 1.0)
  ]

normalize :: [(a, Double)] -> [(a, Double)]
normalize xs = map (second (/total)) xs
  where total = sum$ map snd xs

accum :: [(a, Double)] -> [(a, Double)]
accum xs = zip as $ tail$ scanl (+) 0 bs
  where (as, bs) = unzip xs

distroAll = accum$ normalize weightsAll
distroTerm = accum$ normalize weightsTerm
distroNonTerm = accum$ normalize weightsNonTerm

data GenParams = GenParams
  { forceDepth :: Bool
  , terminalCount :: Int
  , depth :: Int
  , avgDepth :: Double
  , avgDepthDecay :: Double
  } deriving (Show)

randomRM range = state (randomR range)

distroChoice distro = do
  roulette <- state (randomR (0, 1))
  return$ fst$ fromJust$ find (\(x, p) -> p >= roulette) distro

generate :: GenParams -> State StdGen (Ltl, Int)
generate params@GenParams{depth = 0} = distroChoice distroTerm >>= ($ params)
generate params@GenParams{..} | forceDepth == True || depth < ceiling avgDepth =
  distroChoice distroNonTerm >>= ($ params)
generate params = distroChoice distroAll >>= ($ params)

mkVar :: GenParams -> State StdGen (Ltl, Int)
mkVar GenParams{terminalCount} = (, 0) . Var <$> randomRM (0, terminalCount - 1)
mkNot = unary Not
mkNext = unary Next
mkUntil = binary Until
mkAnd = binary (\x y -> And [x, y])

unary op params = (op *** succ) <$> generate (deferParams params)
binary op params@GenParams{..} = do
  (left, succ -> ldepth) <- generate$ deferParams params
    {forceDepth = False, avgDepth = avgDepth*avgDepthDecay}

  (right, succ -> rdepth) <- generate$ deferParams params
    { forceDepth = forceDepth && ldepth < depth
    , avgDepth = 2*avgDepth - fromIntegral ldepth
    }

  return (op left right, max ldepth rdepth)

deferParams params@GenParams{..} = params
  { depth = depth - 1
  , avgDepth = max 0 $ avgDepth - 1
  }

main = do
  rangen <- newStdGen

  [terminalCnt, depth, avgDepth, avgDepthDecay] <- getArgs
  print$ fst$ flip evalState rangen$ generate GenParams
    { forceDepth = True
    , terminalCount = read terminalCnt
    , depth = read depth
    , avgDepth = read avgDepth
    , avgDepthDecay = read avgDepthDecay
    }

