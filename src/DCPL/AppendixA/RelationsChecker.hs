-----------------------------------------------------------------------------
-- |
-- Module      :  DCPL.AppendixA.RelationsChecker
-- Copyright   :  (c) Martin Krauskopf 2012-2018
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  martin.krauskopf@gmail.com
--
-- Takes as an input a set and generates relations on the given set for all
-- possible relations combined from reflexive, symmetric, antisymmetric and
-- transitive properties. That is 16 combinations.
--
-- The output consists of the cardinality (number of elements) and enumeration
-- of all pairs of every relation.
--
-- Helper for Exercise A.1.
--
module Main where

import Data.List(intercalate, intersperse, subsequences, sortBy, groupBy)
import System.Environment(getArgs)

type E = Char -- element type
type BinRel = [(E, E)] -- binary relation type

-- test for a named property of a binary relation
data BinRelPred = BinRelPred { bRel  :: [E] -> BinRel -> Bool
                             , bName :: String
                             }

propPred :: [BinRelPred]
propPred = [ BinRelPred isReflexive "reflexive"
           , BinRelPred isSymmetric "symmetric"
           , BinRelPred isAntisymmetric "antisymmetric"
           , BinRelPred isTransitive "transitive"
           ]

-- Checkers are given relation together with set on which is it defined (needed
-- for reflexivity check)
isReflexive, isSymmetric, isAntisymmetric, isTransitive :: [E] -> BinRel -> Bool
isReflexive xs rel = all (\x -> (x,x) `elem` rel) xs
isSymmetric _ rel = all (\(x,y) -> (y,x) `elem` rel) rel
isAntisymmetric _ rel = all (\(x,y) -> x == y || (y,x) `notElem` rel) rel
isTransitive _ rel = and [ trans pair | pair <- rel ]
  where trans (x,y) = and [ (x,v) `elem` rel | (u,v) <- rel, u == y ]

-- Returns whether a given binary relation on a given set is:
--   [ reflexive, symmetric, antisymmetric, transitive ]
properties :: [E] -> BinRel -> [Bool]
properties set rel = [ bRel pp set rel | pp <- propPred ]

groupedResult :: [E] -> [[(BinRel, [Bool])]]
groupedResult set = groupBy byProperties . sortBy propSorter $ [ (r, properties set r) | r <- allRels ]
  where
    allRels = subsequences [ (x,y) | x <- set, y <- set ] -- size 2^|set|^2 (subsequences == powerset)
    byProperties (_,ps1) (_,ps2) = ps1 == ps2
    propSorter (_,ps1) (_,ps2) = ps1 `compare` ps2


-- Pretty-print the results.
main :: IO ()
main = do
  args <- getArgs
  let set                      = if null args then "abc" else head args
      result                   = groupedResult set
      names                    = map bName propPred
      showProps (_,bs)         = show $ map snd $ filter fst (zip bs names)
      showSet                  = ("{" ++) . (++ "}") . intersperse ','
      showGroup xs@(x:_)       = showProps x ++ "\n\t" ++ intercalate "\n\t" (map (show . fst) xs)
      showGroupLength xs@(x:_) = " " ++ showProps x ++ ": " ++ show (length xs)
   in do
      putStrLn $ "Properties of binary relations defined on a set " ++ showSet set ++ ":"
      mapM_ (putStrLn . showGroupLength) result
      putStrLn "\nIndividual relation:"
      mapM_ (putStrLn . (" " ++) . showGroup) result

