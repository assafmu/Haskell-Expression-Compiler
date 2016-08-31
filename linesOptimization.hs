module LinesOptimization
(
linesOptimize
)
where
import Emitter
import Data.Maybe
import Data.List

linesOptimize :: Int -> [Line] -> [Line]
--linesOptimize _ = id
linesOptimize x lines = (iterate singleLineOptimization lines ) !! x
    where singleLineOptimization lines = maybeReplace lines $ findRemovableLine lines
          maybeReplace l Nothing = l
          maybeReplace l (Just (i,j)) = replaceReference i j l


findReplacement :: Line -> [Line] -> Maybe (Int,Int)
findReplacement (ConstantLine i j) list = fmap asTuple $ find sameLine list
    where sameLine (ConstantLine _ j2) = j==j2
          sameLine _ = False
          asTuple (ConstantLine i2 _) = (i,i2)
findReplacement _ _ = Nothing
          
findRemovableLine :: [Line] -> Maybe (Int,Int)
findRemovableLine list = helper Nothing $ reverse list
    where helper result@(Just _) _ = result
          helper Nothing (x:xs) = helper (findReplacement x xs) xs
          helper Nothing _ = Nothing

replaceReference :: Int -> Int -> [Line] -> [Line]
replaceReference s1 s2 = mapMaybe (replaceReference' s1 s2)

replaceReference' :: Int -> Int -> Line -> Maybe Line
replaceReference' i1 i2 line@(ConstantLine i j) | i1 == i = Nothing
    | otherwise = Just $ line
replaceReference' i1 i2 line@(OperatorLine i t1 op t2) | replaceLHS && replaceRHS = Just $ OperatorLine i newVar op newVar
    | replaceLHS = Just $ OperatorLine i newVar op t2
    | replaceRHS = Just $ OperatorLine i t1 op newVar
    | otherwise = Just $ line
    where replacedVar = innerVar i1
          newVar = innerVar i2
          (replaceLHS,replaceRHS) = (replacedVar == t1,replacedVar == t2)
replaceReference' i1 i2 l = Just l