module Emitter
(
emitTree
)
where 
import Tokeniser
import Data.Maybe
import AST


emitTree :: AST -> String
emitTree tree = result++"x = t"++(show $ i-1)++"\nprint x" where
      (result,i) = output' 1 tree

output' :: Int -> AST -> (String,Int)
output' i (Literal (Number x)) = ("t"++(show i)++"="++(show x)++"\n",i+1)
output' i (Expression (Operator o) (t1:t2:[])) = (before++"t"++(show i'')++"="++"t"++(show $ i'-1)++(displayOperator o)++"t"++(show $ i''-1)++"\n",i''+1) where
     (out1,i') = output' i t1
     (out2,i'') = output' i' t2
     before = out1++out2


displayOperator :: Operator -> String
displayOperator Plus = "+"
displayOperator Minus = "-"
displayOperator Times = "*"
displayOperator Divide = "/"