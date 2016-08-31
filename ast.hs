module AST
(
AST(Expression,Literal,VarReference),
compile,
ruleStep
)
where 
import Tokeniser
import Data.Maybe

data AST = Expression Token [AST] | Literal Token | VarReference String | Open | Close deriving  (Show,Eq)

compile :: [Token] -> (AST,String)
compile (Reference r:Assign:l) = (head $ fst $ buildTree ([], l),r)

buildTree ::  ([AST], [Token]) -> ([AST], [Token])
buildTree (x:[],[]) = ([x],[])
buildTree (stack,tokens) = buildTree $ ruleStep stack tokens

asExpression :: Operator -> AST
asExpression o = Expression (Operator o) []

 -- stack, tokens left   
ruleStep :: [AST] -> [Token] -> ([AST],[Token])
-- Reduce rules - rules which pop expressions from the stack to create fewer expressions
ruleStep (Close:t2:Open:xs) tokens = (t2:xs,tokens)
ruleStep x@(t1:(Expression (Operator o) _):t2:xs) y@((Operator o2):ys) -- Next operator might be of higher priority
     | priority o >= priority o2 = ((Expression (Operator o) [t2,t1]):xs,y)
     | otherwise = (asExpression o2 : x,ys)
ruleStep (t1:(Expression (Operator o) _):t2:xs) l | referable t1 && referable t2= ((Expression (Operator o) [t2,t1]):xs,l) -- Next token either doesn't exist, or isn't an operator
    where referable Open = False
          referable Close = False
          referable _ = True
-- Shift rules - rules which read from the tokens list
ruleStep l (Operator o:xs) = (asExpression o: l,xs)
ruleStep l (Number x:xs) =  (Literal (Number x) : l,xs)
ruleStep l (Reference s:xs) = (VarReference s:l ,xs)
ruleStep l (OpenParen:xs) = (Open : l,xs)
ruleStep l (CloseParen:xs) = (Close : l,xs)
ruleStep (x:[]) [] = ([x],[])

priority :: Operator -> Int
priority Plus = 2
priority Minus = 3
priority Times = 4
priority Divide = 5
priority Exp = 6