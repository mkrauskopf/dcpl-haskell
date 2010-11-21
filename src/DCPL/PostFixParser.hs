-----------------------------------------------------------------------------
-- |
-- Module      :  DCPL.PostFixParser
-- Copyright   :  (c) Martin Krauskopf 2010
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  martin.krauskopf@gmail.com
--
-- Parser for PostFix language from the "Design Concepts in Programming
-- Languages" textbook.
--

module DCPL.PostFixParser

where

import Data.Functor((<$>))
import Text.ParserCombinators.Parsec hiding (spaces)

-- | Commands understood by the PostFix stack language.
data Command = Num Int
             | Lt | Gt | Eq
             | Pop | Swap | Sel | NGet | Exec
             | Sub | Add | Mul | Div | Rem
             | Seq [Command] -- executable sequence
  deriving (Show, Eq)


spaces :: Parser ()
spaces = skipMany1 space

parseList :: Parser Command
parseList = Seq <$> sepBy parseExpr spaces

parseExpr :: Parser Command
parseExpr = parseNumber
        <|> parseInstruction
        <|> parseSequence

parseNumber :: Parser Command
parseNumber = (Num . read) <$> many1 digit

parseInstruction :: Parser Command
parseInstruction = inst "lt" Lt
               <|> inst "gt" Gt
               <|> inst "eq" Eq

               <|> inst "pop" Pop
               <|> inst "swap" Swap
               <|> inst "sel" Sel
               <|> inst "nget" NGet
               <|> inst "exec" Exec

               <|> inst "sub" Sub
               <|> inst "add" Add
               <|> inst "mul" Mul
               <|> inst "div" Div
               <|> inst "rem" Rem
  where
    inst :: String -> Command -> Parser Command
    inst s cmd = try (string s >> return cmd)

parseSequence :: Parser Command
parseSequence = do
    char '('
    seq' <- parseList
    char ')'
    return seq'

