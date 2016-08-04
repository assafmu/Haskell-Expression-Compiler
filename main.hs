import Data.Char
import Tokeniser
import AST
import Emitter
-- flow is String to [Token] to AST to [Line] to [String] to String
main = interact  $ unlines . (map outputLine) . emitTree . compile . tokenise