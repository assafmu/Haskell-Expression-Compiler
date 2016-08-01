module Tokeniser
(
Operator(..),Token(..),tokenise
)
where

import Data.Char
import Text.Regex.Posix

data Operator = Plus | Minus | Times | Divide deriving (Show,Eq,Read)

data Token = Number Int| Operator Operator | OpenParen | CloseParen deriving (Show,Eq)

tokenise :: String -> [Token]
tokenise [] = []
tokenise (' ':xs) = tokenise xs
tokenise ('+':xs) = Operator Plus : tokenise xs
tokenise ('-':xs) = Operator Minus : tokenise xs
tokenise ('*':xs) = Operator Times : tokenise xs
tokenise ('/':xs) = Operator Divide : tokenise xs
tokenise ('(':xs) = OpenParen : tokenise xs
tokenise (')':xs) = CloseParen : tokenise xs
tokenise s = Number (read i) : tokenise xs where
     reg = "[0-9]+"
     (start,i,xs) = (s =~ reg) :: (String,String,String)