import Data.List
import Lexer
import Grammar
import Interpreter
import System.Environment ( getArgs )
import Control.Exception ( catch, ErrorCall )
import System.IO

main :: IO ()
main = catch main' noLex

main' :: IO ()
main' = do fileName <- getArgs 
           sourceText <- readFile (head fileName)
--           putStrLn ("Lexing : " ++ sourceText)
           let lexedProg = queryLang (alexScanTokens sourceText)
           result <- evalStart lexedProg
           putStrLn result
--           putStrLn ("Lexed as " ++ (show lexedProg))

noLex :: ErrorCall -> IO ()
noLex e = do let err =  show e
             hPutStr stderr ("Problem with Lexing : " ++ err)
             return ()