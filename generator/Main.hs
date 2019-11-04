module Main where

import Control.Monad
import Data.List
import System.Environment

import Safe (initMay)

import Ltl (Ltl(..))

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

            variations2 fn =
              [fn x y | x <- greatest, y <- smaller] ++
              [fn y x | x <- greatest, y <- smaller] ++
              [fn x y | x <- greatest, y <- greatest]

main = do
  [terminalCnt] <- getArgs
  forM (generate$ read terminalCnt) $ \ltl -> do
    print ltl
    getLine
