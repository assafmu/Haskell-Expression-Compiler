import Data.Char
import Tokeniser
import AST
import Emitter
main = interact foo

foo :: String -> String
foo = emitTree . compile . tokenise