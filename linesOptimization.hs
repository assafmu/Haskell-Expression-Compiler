module LinesOptimization
(
linesOptimize
)
where
import Emitter
import Data.Maybe

linesOptimize :: [Line] -> [Line]
linesOptimize = id

--Currently unused, soon to be used by linesOptimize
replaceReference :: String -> String -> [Line] -> [Line]
replaceReference s1 s2 = mapMaybe (replaceReference' s1 s2)

replaceReference' :: String -> String -> Line -> Maybe Line
replaceReference' s1 s2 (ConstantLine i j) | s1 == "_t" ++ (show i) = Nothing
    | otherwise = Just $ ConstantLine i j
replaceReference' s1 s2 (AssignmentLine t1 t2) | s1 == t1 = Nothing
    | otherwise = Just $ AssignmentLine t1 t2
replaceReference' s1 s2 (OperatorLine i t1 o t2) | s1 == t1 = Just $ OperatorLine i s2 o t2
    | otherwise = Just $ OperatorLine i t1 o t2
replaceReference' s1 s2 l = Just l