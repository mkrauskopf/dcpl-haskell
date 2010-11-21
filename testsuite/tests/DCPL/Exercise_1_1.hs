-----------------------------------------------------------------------------
-- |
-- Module      :  DCPL.PostFixEvalTest
-- Copyright   :  (c) Martin Krauskopf 2010
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  martin.krauskopf@gmail.com
--
-- Answers question to the exercise 1.1
--

module DCPL.Exercise_1_1

where

import DCPL.PostFixEval

main :: IO ()
main = mapM_ (putStrLn . result) $ zip ['a'..'z']
    [ "10 (swap 2 mul sub) 1 swap exec)"
    , "(5 (2 mul) exec) 3 swap)"
    , "(() exec) exec)"
    , "2 3 1 add mul sel)"
    , "2 3 1 (add) (mul) sel)"
    , "2 3 1 (add) (mul) sel exec)"
    , "0 (2 3 add) 4 sel exec)"
    , "1 (2 3 add) 4 sel exec)"
    , "(5 6 lt) (2 3 add) 4 sel exec)"
    , "(swap exec swap exec) (1 sub) swap (2 mul) swap 3 swap exec)"
    ]
  where
    result (c,s) = c:". (postfix 0 " ++ s ++ "\n" ++ postfix s ++ "\n"

