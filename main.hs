import Data.Char
import Tokeniser
import AST
import Emitter
-- for each line, flow is String to [Token] to AST to [Line] to [String] to String
main = interact  $ unlines . map (compileExpression) . lines
     where compileExpression = unlines . (map outputLine) . emitTree .compile . tokenise