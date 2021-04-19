module Interpreter where
import Lexer
import Grammar
import CSV_Reader
import System.Environment
import Control.Exception
import System.IO

--data Exp = FinalExp [String] RequirementList
--     deriving (Eq,Show) 
--
--data Column = Var String
--      | SkipVar
--     deriving (Eq,Show) 
--
--data Assignment = AsignVarVar String String
--      | AsignVarStr String String
--     deriving (Eq,Show)  
--
--Requirement : str '(' ColumnList ')'           { Table $1 $3 }
--     | var '=' var                             { Eq $1 $3 }
--     | var "!=" var                            { NEq $1 $3 } 
--     | var '=' empty                           { Empty $1 }
--     | var "!=" empty                          { NotEmpty $1 }
--     | var "<-" var                            { AsignVarVar $1 $3 }
--     | var "<-" str                            { AsignVarStr $1 $3 }
--     | if '(' RequirementList ')' then '(' RequirementList ')' else '(' RequirementList ')'  { IfTF $3 $7 $11 }    
--
--type RequirementList = [Requirement]
--type ColumnList = [Column]


--FinalExp [x1,x2] [(Table "A" (x1,x2,_)),(Eq x1 x2),(Empty x2)]

evalStart :: Exp -> IO [[String]]
evalStart (FinalExp bvs rs) = evalExp bvs (evalRequirementList rs)

evalColumnList :: ColumnList  -> [String]
evalColumnList clms = [evalColumn c |c <- clms ]

evalColumn :: Column -> String
evalColumn (Var str) = str
evalColumn (SkipVar) = []

-- evalRequirementList :: RequirementList -> [Row]
evalRequirementList rs = [evalRequirement r |r <- rs ]


--evalRequirement :: Requirement -> 
-- tova nqma da stane s pattern matching
-- zashtoto table-a vrushta drug type
evalRequirement (Table name clms) = do
    file <- fileReadCsv (name++".csv")

    

evalRequirement (Eq f s) =

evalRequirement (NEq f s) =

evalRequirement (Empty s) =

evalRequirement (NotEmpty s) =

evalRequirement (IfTF rle rlt rlf)
   | evaluated = evalRequirement rlt
   | otherwise = evalRequirement rlf
      where evaluated = evalRequirementListIF rle


-- This is the data type for assigned values to variables
--           varName, value
data Row = [(String,String)]
data File = [Row]
-- Bazingaringa

evalExp :: [String] -> RequirementList -> IO [[String]]
evalExp bvs rs = do
    


main :: IO ()
main = catch main' noLex

main' = do (fileName : _ ) <- getArgs 
           sourceText <- readFile fileName
           putStrLn ("Lexing : " ++ sourceText)
           let lexedProg = queryLang (alexScanTokens sourceText)
           putStrLn ("Lexed as " ++ (show lexedProg))

noLex :: ErrorCall -> IO ()
noLex e = do let err =  show e
             hPutStr stderr ("Problem with Lexing : " ++ err)
             return ()


--eval1


evalRequirements (Table str ) = 

