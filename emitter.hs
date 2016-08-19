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

data Line = ConstantLine Int Int | OperatorLine Int String Operator String |AssignmentLine String String| PrintLine String deriving (Eq,Show)

emitTree :: (AST,String) -> [Line]
emitTree (tree,s) = result++[AssignmentLine s ("_t"++(show $ i-1)),PrintLine s] where
      (result,i) = output' 1 tree

output' :: Int -> AST -> ([Line],Int)
output' i (Literal (Number x)) = ([ConstantLine i x],i+1)
output' i (VarReference s) = ([],i)
output' i (Expression (Operator o) (t1:t2:[])) = (before ++ [OperatorLine i'' (helper (i'-1) t1) o (helper (i''-1) t2)],i''+1) where
     (lines1,i') = output' i t1
     (lines2,i'') = output' i' t2
     before = lines1++lines2
     helper :: Int -> AST -> String
     helper i (VarReference s1) = s1
     helper i (Literal (Number x1)) = "_t"++(show i)
     helper i (Expression _ _) = "_t"++(show i)

outputLine :: Line -> String
outputLine (ConstantLine i x) = "_t"++(show i)++"="++(show x)
outputLine (OperatorLine i s1 op s2) = innerVar i++"="++s1++displayOperator op++s2
outputLine (PrintLine s) = "print "++s
outputLine (AssignmentLine to from) = to ++ "=" ++ from

innerVar :: Int -> String
innerVar i = "_t"++(show i)

displayOperator :: Operator -> String
displayOperator Plus = "+"
displayOperator Minus = "-"
displayOperator Times = "*"
displayOperator Divide = "/"