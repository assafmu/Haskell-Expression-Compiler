module Emitter
(
emitTree,
emitTrees,
Line(..),
outputLine,
innerVar
)
where 
import Tokeniser
import Data.Maybe
import AST

data Line = ConstantLine Int Int | OperatorLine Int String Operator String | AssignmentLine String String| PrintLine String deriving (Eq,Show)

emitTrees :: [(AST,String)] -> [Line]
emitTrees = concat . map fst . scanl helper ([],1)
    where helper (_,startIndex) (tree,var) = emitTree startIndex (tree,var)

emitTree :: Int -> (AST,String) -> ([Line],Int)
emitTree start (tree,s) = (result++[AssignmentLine s (innerVar $ i-1 ),PrintLine s],i) where
      (result,i) = output' start tree

output' :: Int -> AST -> ([Line],Int)
output' i (Literal (Number x)) = ([ConstantLine i x],i+1)
output' i (VarReference s) = ([AssignmentLine (innerVar i) s],i+1)
output' i (Expression (Operator o) (t1:t2:[])) = (before ++ [OperatorLine i'' (helper (i'-1) t1) o (helper (i''-1) t2)],i''+1) where
     (lines1,i') = output' i t1
     (lines2,i'') = output' i' t2
     before = lines1++lines2
     helper :: Int -> AST -> String
     helper i (VarReference s1) = s1
     helper i (Literal (Number x1)) = innerVar i
     helper i (Expression _ _) = innerVar i

outputLine :: Line -> String
outputLine (ConstantLine i x) = (innerVar i)++"="++(show x)
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
displayOperator Exp = "**"