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

-- This is the data type for assigned values to variables
--           varName, value
-- (String,String)   ("x1", "mencho")
type Row = [(String,String)]
type File = [Row]


evalStart :: Exp -> IO [[String]]
evalStart (FinalExp bvs rs) =  evalRequirementsList rs []

evalColumnList :: ColumnList  -> [String]
evalColumnList clms = [evalColumn c |c <- clms ]

evalColumn :: Column -> String
evalColumn (Var str) = str
evalColumn (SkipVar) = []

-- Eval all requirements
evalRequirementList :: RequirementList -> IO File -> IO File
evalRequirementList (r:rs) currentFile = evalRequirementList rs result
    where result = evalRequirement r currentFile

-- Eval requirements 1 by 1
evalRequirement :: Requirement -> IO File -> IO File
evalRequirement (Table name clms) currentFile = do
    file <- fileReadCsv (name++".csv")   -- this is a list of rows
    let vars = evalColumnList clms 
        newTable = map (zip vars) file
    if currentFile == [] 
        then return newTable
        else return (conjunctTable currentFile newTable)


-- 1 function to evaluate all of if requirements (rle) for each row 
  -- 1 function to evaluate then and else (if true it evaluates rlt) (if false it evaluates rlf)
  -- then after the evaluation the function returns the row (either full of values or empty)
evalRequirement (IfTF rle rlt rlf)
   | evaluated = evalRequirement rlt
   | otherwise = evalRequirement rlf
      where evaluated = evalRequirementListIF rle 

-- filter all in the currentFile ... works for 
evalRequirement req currentFile = do
    return filter (checkRequirement req) currentFile

-- do conjunction here
conjunctTable ::  File -> File -> File
conjunctTable [] file2 = []
conjunctTable file1 [] = file1
conjunctTable (row:rows) file2 = map (row ++) file2 ++ conjunctTable rows file2
--                                   concatRow row


--concatRow :: Row -> Row -> Row
--concatRow [] row2 = row2
--concatRow row1 [] = row1
--concatRow row1 row2 = row1 ++ row2


--TODO:

-- eval Table (conjunction)

-- eval if requirements ()

-- get/assign Value function

-- print function?

-- matching names function 




checkRequirement :: Requirement -> Row -> Bool
checkRequirement (Eq v1 v2) row = (fetchVar v1 row) == (fetchVar v2 row) 
checkRequirement (NEq v1 v2) row = (fetchVar v1 row) /= (fetchVar v2 row) 
checkRequirement (Empty v1) row = (fetchVar v1 row) == ""
checkRequirement (NotEmpty v1) row = (fetchVar v1 row) /= "" 
checkRequirement _ row = True

-- function to fetch the value of a variable
fetchVar :: Var -> Row -> String 
fetchVar varName [] = ""
fetchVar varName ((var,val):restOfRow) 
    | varName == var = val 
    | otherwise = fetchVar varName restOfRow


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


--evalRequirements (Table str ) = 

