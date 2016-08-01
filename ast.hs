module AST
(
AST(..),
compile,
ruleStep
)
where 
import Tokeniser
import Data.Maybe

data AST = Expression Token [AST] | Literal Token | Empty deriving  (Show,Eq)

compile :: [Token] -> AST
compile l = head $ fst $ buildTree ([], l)

buildTree ::  ([AST], [Token]) -> ([AST], [Token])
buildTree (x:[],[]) = ([x],[])
buildTree (stack,tokens) = buildTree $ ruleStep stack tokens

asExpression :: Operator -> AST
asExpression o = Expression (Operator o) []

 -- stack, tokens left   
ruleStep :: [AST] -> [Token] -> ([AST],[Token])
ruleStep (x:[]) [] = ([x],[])
ruleStep ((Expression CloseParen _):t2:(Expression OpenParen _):xs) tokens = (t2:xs,tokens)
ruleStep (t1:(Expression (Operator o) _):t2:xs) [] = ((Expression (Operator o) [t2,t1]):xs,[])
ruleStep x@(t1:(Expression (Operator o) _):t2:xs) y@((Operator o2):ys)
     | priority o >= priority o2 = ((Expression (Operator o) [t2,t1]):xs,y)
     | otherwise = (asExpression o2 : x,ys)
ruleStep l ((Operator o):xs) = (asExpression o: l,xs)
ruleStep l ((Number x):xs) =  (Literal (Number x) : l,xs)
ruleStep l (OpenParen:xs) = (Expression OpenParen [] : l,xs)
ruleStep l (CloseParen:xs) = (Expression CloseParen [] : l,xs)


priority :: Operator -> Int
priority Plus = 2
priority Minus = 3
priority Times = 4
priority Divide = 5