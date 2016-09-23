import Tokeniser
import AST
import Emitter
import TreeOptimization
import LinesOptimization


main = interact $ unlines . map outputLine . linesOptimize . outputTrees . treesOptimize . map compileLine . lines
    where compileLine = compile . tokenise
          outputTrees =  emitTrees
