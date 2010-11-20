-----------------------------------------------------------------------------
-- |
-- Module      :  DCPL.PostFixEval
-- Copyright   :  (c) Martin Krauskopf 2010
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  martin.krauskopf@gmail.com
--
-- Evaluator for PostFix from the "Design Concepts in Programming Languages"
-- textbook.
--

module DCPL.PostFixEval

where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Functor((<$>))
import DCPL.PostFixParser


data PostFixError = EvalError String
                  | PostFixParseError ParseError

type ThrowsError = Either PostFixError

instance Show PostFixError where
  show (EvalError message) = "PostFixError: " ++ message
  show (PostFixParseError parseError) = show parseError


-- | PostFix 'program' datastructure.
data Program = Program Int [Command] Stack


-- | Stack is represented as a list where first element of the list represents
-- top element at the stack.
--
-- Valid values on stack are only `Num Int' and `Seq [Command]' commands.
type Stack = [Command]


-- | User-level function for convenient usage.
postfix :: String -> String
postfix toEval = case evalToInt toEval of
                     Right res  -> "  -> " ++ show res
                     Left err   -> show err


evalToInt :: String
          -> ThrowsError Int
evalToInt toEval = case evalString toEval of
    Left err         -> Left err
    Right (Num n:xs) -> Right n
    Right stack      ->
      evalError $ "The value at the top of the final is not an integer. Final stack:\n -> "
               ++ show stack


-- | Evaluates PostFix program given as a string.
evalString :: String            -- ^ commands to be executed as a string
           -> ThrowsError Stack -- ^ result
evalString toEval = case parse parseList "postfix" toEval of
                      Left err     -> Left $ PostFixParseError err
                      Right parsed -> eval [parsed, Exec] []


-- | Evaluates PostFix program given as a list of commands.
eval :: [Command]         -- ^ commands to be executed
     -> Stack             -- ^ currenct stack
     -> ThrowsError Stack -- ^ result
eval [] stack = Right stack
eval (c:cs) stack = case c of
  n@(Num _) -> eval cs $ n:stack
  s@(Seq _) -> eval cs $ s:stack
  Add       -> binEval (+) stack cs
  Sub       -> binEval (-) stack cs
  Mul       -> binEval (*) stack cs
  Div       -> binEval0 quot stack cs
  Rem       -> binEval0 rem stack cs
  Pop       -> pop cs stack
  Swap      -> swap cs stack
  Sel       -> sel cs stack
  Exec      -> exec cs stack
  _         -> evalError $ "Unknown command: " ++ show c
  where
    stackS    = "(stack: " ++ show stack ++ ")"


pop :: [Command] -> Stack -> ThrowsError Stack
pop _ []      = evalError "Cannot perform 'pop' operation on empty stack"
pop cs (x:xs) = eval cs xs


swap :: [Command] -> Stack -> ThrowsError Stack
swap cs (x:y:xs) = eval cs (y:x:xs)
swap _ stack     = evalError $ "Not enough values to swap. Current stack:\n  " ++ show stack


sel :: [Command] -> Stack -> ThrowsError Stack
sel cs (x:y:Num z:xs) = let res = if z == 0 then x else y
                        in eval cs (res:xs)
sel _ stack = evalError $ "Not enough values to perform selection " ++ show stack


exec :: [Command] -> Stack -> ThrowsError Stack
exec cs (Seq cmds:xs) = eval (cmds ++ cs) xs
exec cs stack@(_:xs)  = evalError $ "Seq expected on top of the stack while performing Exec." ++ show stack
exec _ stack          = evalError $ "Not enough values to perform sequence execution " ++ show stack


binEval0 :: (Int -> Int -> Int) -> Stack -> [Command] -> ThrowsError Stack
binEval0 _ (Num 0:Num y:xs) _ = evalError "Divide by zero"
binEval0 op stack cs = binEval op stack cs


binEval :: (Int -> Int -> Int) -> Stack -> [Command] -> ThrowsError Stack
binEval binOp (Num x:Num y:xs) cs = eval cs ((Num $ binOp y x):xs)
binEval _ stack _ = evalError $ "Not enough numbers for binary operation (stack: " ++ show stack ++ ")"


evalError :: String -> ThrowsError a
evalError = Left . EvalError

