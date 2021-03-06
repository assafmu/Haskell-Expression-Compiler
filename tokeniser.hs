module Tokeniser
(
Operator(..),Token(..),tokenise
)
where

import Data.Char
import Text.Regex.Posix

data Operator = Plus | Minus | Times | Divide | Exp deriving (Show,Eq,Read)

data Token = Number Int| Operator Operator | OpenParen | CloseParen | Reference String | Assign deriving (Show,Eq)

--Not a direct map, due to numbers potentially taking more than 1 character
tokenise :: String -> [Token]
tokenise [] = []
tokenise (' ':xs) = tokenise xs
tokenise ('+':xs) = Operator Plus : tokenise xs
tokenise ('-':xs) = Operator Minus : tokenise xs
tokenise ('*':xs) = Operator Times : tokenise xs
tokenise ('/':xs) = Operator Divide : tokenise xs
tokenise ('^':xs) = Operator Exp : tokenise xs
tokenise ('(':xs) = OpenParen : tokenise xs
tokenise (')':xs) = CloseParen : tokenise xs
tokenise ('=':xs) = Assign : tokenise xs
tokenise s | start == "" = Number (read i) : tokenise xs where
     reg = "[0-9]+"
     (start,i,xs) = (s =~ reg) :: (String,String,String)
tokenise s = Reference ref : tokenise xs where
     reg = "[a-zA-Z0-9]+"
     (start,ref,xs) = (s =~ reg) :: (String,String,String)
