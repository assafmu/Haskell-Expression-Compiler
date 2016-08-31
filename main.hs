import Tokeniser
import AST
import Emitter
import TreeOptimization
import LinesOptimization


optimizationCount = 10
main = interact $ unlines . map outputLine . linesOptimize optimizationCount . outputTrees . treesOptimize . map compileLine . lines
    where compileLine = compile . tokenise
          outputTrees =  emitTrees
