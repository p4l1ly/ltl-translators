{-# LANGUAGE ViewPatterns, RecordWildCards, NamedFieldPuns, TupleSections, ScopedTypeVariables #-}
module Main where

import Control.Arrow
import Control.Monad.State
import Data.List
import Data.Maybe
import System.Environment
import System.Random

import Ltl (Ltl(..))

-- weightsAll =
--   [ (mkAnd, 1.2)
--   , (mkOr, 0.8)
--   , (mkNot, 1.0)
--   , (mkNext, 1.0)
--   , (mkUntil, 1.0)
--   , (mkVar, 6.0)
--   ]
-- 
-- weightsTerm =
--   [ (mkVar, 3.0)
--   ]
-- 
-- weightsNonTerm =
--   [ (mkAnd, 1.2)
--   , (mkOr, 0.8)
--   , (mkNot, 1.0)
--   , (mkNext, 1.0)
--   , (mkUntil, 1.0)
--   ]

-- m7:
weightsAll =
  [ (mkAnd, 1.5)
  , (mkOr, 0.9)
  , (mkNot, 0.5)
  , (mkNext, 2.3)
  , (mkUntil, 1.0)
  , (mkVar, 3.0)
  , (mkTrue, 0.1)
  , (mkFalse, 0.1)
  ]

weightsTerm =
  [ (mkVar, 3.0)
  , (mkTrue, 0.1)
  , (mkFalse, 0.1)
  ]

weightsNonTerm =
  [ (mkAnd, 1.5)
  , (mkOr, 0.9)
  , (mkNot, 1.0)
  , (mkNext, 2.3)
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

distroChoice :: [(b, Double)] -> StateT StdGen IO b
distroChoice distro = do
  roulette <- randomRM (0, 1)
  return$ fst$ fromJust$ find (\(_, p) -> p >= roulette) distro

generate :: GenParams -> StateT StdGen IO (Ltl, Int)
generate params@GenParams{depth = 0} = distroChoice distroTerm >>= ($ params)
generate params@GenParams{..} | forceDepth || depth < ceiling avgDepth =
  distroChoice distroNonTerm >>= ($ params)
generate params = distroChoice distroAll >>= ($ params)

mkVar :: GenParams -> StateT StdGen IO (Ltl, Int)
mkVar GenParams{terminalCount} = (, 0) . Var <$> randomRM (0, terminalCount - 1)
mkTrue _ = return (LTrue, 0)
mkFalse _ = return (LFalse, 0)
mkNot = unary Not
mkNext = unary Next
mkUntil = binary Until
mkAnd = binary (\x y -> And [x, y])
mkOr = binary (\x y -> Or [x, y])

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

  [ terminalCntStr
    , depthStr
    , avgDepthRatioStr
    , avgDepthDecayStr
    , terminalCntDStr
    , depthDStr
    , avgDepthRatioDStr
    , avgDepthDecayDStr
    , stepDStr
    , countStr
    , stopGrowStr
    ] <- getArgs

  let terminalCnt = read terminalCntStr :: Double
  let depth = read depthStr :: Double
  let avgDepthRatio = read avgDepthRatioStr :: Double
  let avgDepthDecay = read avgDepthDecayStr :: Double

  let terminalCntD = read terminalCntDStr :: Double
  let depthD = read depthDStr :: Double
  let avgDepthRatioD = read avgDepthRatioDStr :: Double
  let avgDepthDecayD = read avgDepthDecayDStr :: Double
  let stepD = read stepDStr :: Double

  let stopGrow = read stopGrowStr :: Double
  let count = read countStr :: Int

  flip evalStateT rangen$
    forM ([1..stopGrow] ++ replicate count stopGrow)$ \i -> do
      let
        step = stepD ** (i - 1)
        logi = step * logBase 2 i
        terminalCntI = terminalCnt + terminalCntD * logi
        depthI = depth + depthD * logi
        avgDepthRatioI = avgDepthRatio + avgDepthRatioD * logi
        avgDepthDecayI = avgDepthDecay + avgDepthDecayD * logi

      (ltl, _) <- generate GenParams
        { forceDepth = True
        , terminalCount = round terminalCntI
        , depth = round depthI
        , avgDepth = depthI * avgDepthRatioI
        , avgDepthDecay = avgDepthDecayI
        }
      lift$ print ltl
