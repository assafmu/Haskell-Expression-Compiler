module TreeOptimization
(
treesOptimize
)
where
import AST
treesOptimize :: [(AST,String)] -> [(AST,String)]
treesOptimize = id
