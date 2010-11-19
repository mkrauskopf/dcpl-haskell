-----------------------------------------------------------------------------
-- |
-- Module      :  DCPL.PostFixParserTest
-- Copyright   :  (c) Martin Krauskopf 2010
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  martin.krauskopf@gmail.com
--
-- Tests for DCPL.PostFixParser module.
--

module DCPL.PostFixParserTest

where

import DCPL.PostFixParser
import System.Exit(exitWith, ExitCode(..))
import Test.HUnit.Base
import Test.HUnit.Text(runTestTT)
import Text.ParserCombinators.Parsec.Prim(parse)

-- Compose test cases from the given assertion and runs them. Reports result to
-- the terminal using @runTests@ HUnit fucntion.
runTests :: [Assertion] -> IO Counts
runTests = runTestTT . TestList . map TestCase


assertParsed :: String -> Command -> Assertion
assertParsed toParse expected = case parse parseList "postfix" toParse of
    Left err -> assertFailure $ show err
    Right parsed -> assertEqual "parser failed" expected parsed


main = runTests
    [ assertParsed "1 2 add" $ Seq [Num 1,Num 2,Add]
    , assertParsed "1 2 sub" $ Seq [Num 1,Num 2,Sub]
    , assertParsed "1 2 swap" $ Seq [Num 1,Num 2,Swap]
    , assertParsed "(1 2 add) (3 4 mul) sel exec"
        $ Seq [Seq [Num 1,Num 2,Add],Seq [Num 3,Num 4,Mul],Sel,Exec]
    ]

