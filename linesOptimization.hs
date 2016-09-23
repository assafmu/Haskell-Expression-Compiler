module LinesOptimization
--(
--linesOptimize
--)
where
import Emitter
import Data.Maybe
import Data.List

linesOptimize = optimizeOperatorLines 10 . optimizeConstants 10



optimizeConstants :: Int -> [Line] -> [Line]
optimizeConstants = optimize findConstantReplacement replaceConstantLine

optimizeOperatorLines :: Int -> [Line] -> [Line]
optimizeOperatorLines = optimize findOperatorReplacement replaceOperatorLine

optimize :: ReplacementFinderFunction -> LineReplacementFunction -> Int -> [Line] -> [Line]
--optimizeConstants _ _ _ = id
optimize finder replacer x lines = (iterate singleLineOptimization lines ) !! x
    where singleLineOptimization lines = maybeReplace lines $ findRemovableLine finder lines
          maybeReplace l Nothing = l
          maybeReplace l (Just (i,j)) = replaceReference replacer i j l


findRemovableLine :: ReplacementFinderFunction -> [Line] -> Maybe (Int,Int)
findRemovableLine func list = helper Nothing $ reverse list
    where helper result@(Just _) _ = result
          helper Nothing (x:xs) = helper (func x xs) xs
          helper Nothing _ = Nothing

type ReplacementFinderFunction = Line -> [Line] -> Maybe (Int,Int)
          
findConstantReplacement :: ReplacementFinderFunction
findConstantReplacement (ConstantLine i j) list = fmap asTuple $ find sameLine list
    where sameLine (ConstantLine _ j2) = j==j2
          sameLine _ = False
          asTuple (ConstantLine i2 _) = (i,i2)
findConstantReplacement _ _ = Nothing

findOperatorReplacement :: ReplacementFinderFunction
findOperatorReplacement (OperatorLine i t1 op t2) list = fmap asTuple $ find sameLine list
    where sameLine (OperatorLine _ t1' op' t2') = t1==t1' && op==op' && t2==t2'
          sameLine _ = False
          asTuple (OperatorLine i2 _ _ _) = (i,i2)
findOperatorReplacement _ _ = Nothing

replaceReference :: LineReplacementFunction -> Int -> Int -> [Line] -> [Line]
replaceReference func s1 s2 = mapMaybe (func s1 s2)

type LineReplacementFunction = Int -> Int -> Line -> Maybe Line

replaceConstantLine :: LineReplacementFunction
replaceConstantLine i1 i2 line@(ConstantLine i j) | i1 == i = Nothing
    | otherwise = Just $ line
replaceConstantLine i1 i2 line@(OperatorLine i t1 op t2) | replaceLHS && replaceRHS = Just $ OperatorLine i newVar op newVar
    | replaceLHS = Just $ OperatorLine i newVar op t2
    | replaceRHS = Just $ OperatorLine i t1 op newVar
    | otherwise = Just $ line
    where replacedVar = innerVar i1
          newVar = innerVar i2
          (replaceLHS,replaceRHS) = (replacedVar == t1,replacedVar == t2)
replaceConstantLine _ _ l = Just l

replaceOperatorLine :: LineReplacementFunction
replaceOperatorLine i1 i2 line@(OperatorLine i t1 op t2) | i == i1 = Nothing
    | replaceLHS && replaceRHS = Just $ OperatorLine i newVar op newVar
    | replaceLHS = Just $ OperatorLine i newVar op t2
    | replaceRHS = Just $ OperatorLine i t1 op newVar
    | otherwise = Just $ line
    where replacedVar = innerVar i1
          newVar = innerVar i2
          (replaceLHS,replaceRHS) = (replacedVar == t1,replacedVar == t2)
replaceOperatorLine _ _ l = Just l