-----------------------------------------------------------------------------
-- |
-- Module      :  DCPL.PostFixEvalTest
-- Copyright   :  (c) Martin Krauskopf 2010
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  martin.krauskopf@gmail.com
--
-- Tests for DCPL.PostFixEval module.
--

module DCPL.PostFixEvalTest

where

import DCPL.PostFixEval
import DCPL.PostFixParser
import System.Exit(exitWith, ExitCode(..))
import Test.HUnit.Base
import Test.HUnit.Text(runTestTT)
import Text.ParserCombinators.Parsec.Prim(parse)

-- Compose test cases from the given assertion and runs them. Reports result to
-- the terminal using @runTests@ HUnit fucntion.
runTests :: [Assertion] -> IO Counts
runTests = runTestTT . TestList . map TestCase


assertEvaluatedList :: [(String, [Command])] -> [Assertion]
assertEvaluatedList = map $ uncurry assertEvaluated

assertEvaluated :: String -> Stack -> Assertion
assertEvaluated toEval expected = case evalString toEval of
    Left err -> assertFailure $ show err
    Right parsed -> assertEqual "parser failed" expected parsed


assertTopStackNumberList :: [(String, Int)] -> [Assertion]
assertTopStackNumberList = map $ uncurry assertTopStackNumber

assertTopStackNumber :: String -> Int -> Assertion
assertTopStackNumber toEval expected = case evalString toEval of
    Left err -> assertFailure $ show err
    Right parsed -> assertEqual "parser failed" (Num expected) $ head parsed


assertPostFixesErrors :: [String] -> [Assertion]
assertPostFixesErrors = map assertPostFixError

-- Asserts that the given PostFix program ends up with an error. Program is run
-- without arguments.
assertPostFixError :: String -> Assertion
assertPostFixError toEval =
  case result of
    Right res  -> assertFailure $ errorMsg res
    _          -> return ()
  where result = evalString toEval
        message = "commands '" ++ toEval ++ "' -> " ++ show result
        errorMsg res = "expected error from '" ++ toEval ++ "', got result: " ++ show res


main = runTests
    ( assertTopStackNumberList
        -- (program, expected result on the top of the stack)
        [ ("1 2 add", 3)
        , ("1 2 sub", (-1))
        , ("7 (2 mul) exec", 14)
        , ("1 2 3", 3)
        , ("1 2 3 pop", 2)
        , ("1 2 swap 3 pop", 1)
        ]
   ++ assertPostFixesErrors
        -- erroneous program
        [ "1 swap"
        , "1 pop pop"
        , "3 swap"
        , "1 3 sel"
        , "(5 (2 mul) exec) 3 swap"
        , "3 4 mul add"
        , "5 4 4 sub div"
        ]
    )

