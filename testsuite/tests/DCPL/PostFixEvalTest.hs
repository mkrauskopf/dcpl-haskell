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
import Test.HUnit.Base
import Test.HUnit.Text(runTestTT)

-- Compose test cases from the given assertion and runs them. Reports result to
-- the terminal using @runTests@ HUnit fucntion.
runTests :: [Assertion] -> IO Counts
runTests = runTestTT . TestList . map TestCase


assertEvaluatedList :: [(String, [Command])] -> [Assertion]
assertEvaluatedList = map $ uncurry assertEvaluated

assertEvaluated :: String -> Stack -> Assertion
assertEvaluated toEval expected = either
    (assertFailure . show)
    (assertEqual "parser failed" expected)
    (evalString toEval)


assertTopStackNumberArgsList :: [([Int], String, Int)] -> [Assertion]
assertTopStackNumberArgsList = map uncurriedAPS
  where uncurriedAPS (a,b,c) = assertTopStackNumberArgs a b c

-- Asserts that the given PostFix program run with the initial given arguments
-- evaluates to the given result.
assertTopStackNumberArgs :: [Int]     -- ^ initial arguments
                         -> String    -- ^ PostFix program
                         -> Int       -- ^ expected result
                         -> Assertion -- ^ resulting Assertion
assertTopStackNumberArgs args toEval expected = either
    (assertFailure . show)
    (assertEqual "parser failed" (Num expected) . head)
    (evalStringArgs args toEval)


assertTopStackNumberList :: [(String, Int)] -> [Assertion]
assertTopStackNumberList = map $ uncurry assertTopStackNumber

assertTopStackNumber :: String -> Int -> Assertion
assertTopStackNumber = assertTopStackNumberArgs []


assertPostFixesErrors :: [String] -> [Assertion]
assertPostFixesErrors = map assertPostFixError

-- Asserts that the given PostFix program ends up with an error. Program is run
-- without arguments.
assertPostFixError :: String -> Assertion
assertPostFixError toEval =
  case result of
    Right res  -> assertFailure $ errorMsg res
    _          -> return ()
  where result = evalToInt toEval
        errorMsg res = "expected error from '" ++ toEval ++ "', got result: " ++ show res


main :: IO Counts
main = runTests
    ( assertTopStackNumberList
        -- (program, expected result on the top of the stack)
        [ ("1 2 add", 3)
        , ("1 2 sub", (-1))
        , ("7 (2 mul) exec", 14)
        , ("1 2 3", 3)
        , ("1 2 3 pop", 2)
        , ("1 2 swap 3 pop", 1)
        , ("5 1 nget mul", 25)
        , ("3 4 lt", 1)
        , ("5 4 lt", 0)
        , ("5 4 gt", 1)
        , ("3 3 eq 10 add", 11)

         -- x c b a
        , ("2 5 4 3 4 nget 5 nget mul mul swap 4 nget mul add add", 25) -- a*(x^2) + b*x + c
        ]

   ++ assertTopStackNumberArgsList
        -- (initial arguments, program, expected result)
        [ ([3,4], "", 3)
        , ([3,4], "swap", 4)
        , ([3,4,5], "pop swap", 5)
        , ([3], "4 sub", -1)
        , ([3], "4 add 5 mul 6 sub 7 div", 4)
        , ([7,6,5,4,3], "add mul sub swap div", -20)
        , ([300,20,1], "4000 swap pop add", 4020)
        , ([3,7], "add 2 div", 5) -- An averaging program.
        , ([17], "3 div", 5)
        , ([17], "3 rem", 2)
        , ([1], "3 7 (sel) exec", 3)
        , ([1], "3 7 sel", 3)
        , ([1], "(1 2 add) (3 4 mul) sel exec", 3)
        , ([3], "4 lt", 1)
        , ([5], "4 lt", 0)
        , ([3], "4 lt 10 add", 11)
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
        , "1 nget"
        , "(2 1 sub) nget mul"
        , "(2 1 sub) 1 nget mul"
        ]
    )

