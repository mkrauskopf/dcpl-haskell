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

import Control.Monad.Error(Error, strMsg, throwError)
import DCPL.PostFixParser
import Text.ParserCombinators.Parsec hiding (spaces)


data PostFixError = EvalError String
                  | PostFixParseError ParseError

type ThrowsError = Either PostFixError

instance Error PostFixError where
  strMsg = EvalError

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
postfix toEval = either show show (evalToInt toEval)


evalToInt :: String
          -> ThrowsError Int
evalToInt toEval = do
  res <- evalString toEval
  case res of
    (Num n:_) -> return n
    stack     ->
      evalErrorS "The value at the top of the final stack is not an integer" stack


-- | Evaluates PostFix program given as a string.
evalString :: String            -- ^ commands to be executed as a string
           -> ThrowsError Stack -- ^ result
evalString toEval = either
  (throwError . PostFixParseError)
  (\parsed -> eval [parsed, Exec] [])
  (parse parseList "postfix" toEval)


-- | Evaluates PostFix program given as a list of commands.
eval :: [Command]         -- ^ commands to be executed
     -> Stack             -- ^ currenct stack
     -> ThrowsError Stack -- ^ result
eval [] stack = return stack
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
  NGet      -> nget cs stack
  Exec      -> exec cs stack
  _         -> evalError $ "Unknown command: " ++ show c


pop :: [Command] -> Stack -> ThrowsError Stack
pop _ []      = evalError "Cannot perform 'pop' operation on empty stack"
pop cs (_:xs) = eval cs xs


swap :: [Command] -> Stack -> ThrowsError Stack
swap cs (x:y:xs) = eval cs (y:x:xs)
swap _ stack     = evalErrorS "Not enough values to swap" stack


sel :: [Command] -> Stack -> ThrowsError Stack
sel cs (x:y:Num z:xs) = let res = if z == 0 then x else y
                        in eval cs (res:xs)
sel _ stack = evalErrorS "Not enough values to perform selection" stack


nget :: [Command] -> Stack -> ThrowsError Stack
nget cs (Num i:xs)
  | length xs >= i = case xs!!(i-1) of
      n@(Num _) -> eval cs (n:xs)
      _         -> evalErrorS ("Not a numeral on the index " ++ show i ++ " of the stack: ") xs
  | otherwise = evalErrorS ("Index " ++ show i ++ " too large for nget operation. Stack: ") xs
nget _ stack = evalErrorS "Non-integer value on the top of the stack during 'nget'" stack


exec :: [Command] -> Stack -> ThrowsError Stack
exec cs (Seq cmds:xs) = eval (cmds ++ cs) xs
exec _ stack@(_:_)    = evalErrorS "Seq expected on top of the stack while performing Exec" stack
exec _ stack          = evalErrorS "Not enough values to perform sequence execution" stack


binEval0 :: (Int -> Int -> Int) -> Stack -> [Command] -> ThrowsError Stack
binEval0 _ (Num 0:Num _:_) _ = evalError "Divide by zero"
binEval0 op stack cs = binEval op stack cs


binEval :: (Int -> Int -> Int) -> Stack -> [Command] -> ThrowsError Stack
binEval binOp (Num x:Num y:xs) cs = eval cs ((Num $ binOp y x):xs)
binEval _ stack _ = evalErrorS "Not enough numbers for binary operation" stack


-- | Returns 'EvalError' with the given message.
evalError :: String -> ThrowsError a
evalError = throwError . EvalError


-- | Returns 'EvalError' with pretty-printed error message containing the given
-- message and stack.
evalErrorS :: String -> Stack -> ThrowsError a
evalErrorS errMsg stack = evalError $ errMsg ++ "\n - stack: " ++ show stack

