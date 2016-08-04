module Emitter
(
emitTree,
Line(..),
outputLine
)
where 
import Tokeniser
import Data.Maybe
import AST

data Line = ConstantLine Int Int | OperatorLine Int Int Operator Int | PrintLine Int deriving (Eq,Show)

emitTree :: AST -> [Line]
emitTree tree = result++[PrintLine $ i-1] where
      (result,i) = output' 1 tree

output' :: Int -> AST -> ([Line],Int)
output' i (Literal (Number x)) = ([ConstantLine i x],i+1)
output' i (Expression (Operator o) (t1:t2:[])) = (before ++ [OperatorLine i'' (i'-1) o (i''-1)],i''+1) where
     (lines1,i') = output' i t1
     (lines2,i'') = output' i' t2
     before = lines1++lines2 

outputLine :: Line -> String
outputLine (ConstantLine i x) = 't':(show i)++"="++(show x)
outputLine (OperatorLine i t1 op t2) = 't':(show i)++"=t"++(show t1)++displayOperator op ++ "t"++(show t2)
outputLine (PrintLine i) = "print t"++(show i)

displayOperator :: Operator -> String
displayOperator Plus = "+"
displayOperator Minus = "-"
displayOperator Times = "*"
displayOperator Divide = "/"