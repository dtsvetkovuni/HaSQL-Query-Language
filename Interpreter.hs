module Interpreter where
import Lexer
import Grammar
import CSV_Reader
import System.Environment
import Control.Exception
import System.IO
import Data.List

type Row = [(String,String)]
type File = [Row]


evalStart :: Exp -> IO String
evalStart (FinalExp bvs rs) =do
    evaluated <- evalRequirementList rs []
    return (showResult bvs evaluated)

evalColumnList :: ColumnList  -> [String]
evalColumnList clms = [evalColumn c |c <- clms ]

evalColumn :: Column -> String
evalColumn (Var str) = str
evalColumn SkipVar = []

-- Eval all requirements
evalRequirementList :: RequirementList -> File -> IO File
evalRequirementList [] currentFile = do return currentFile
evalRequirementList (r:rs) currentFile = do
    result <- evalRequirement r currentFile
    evalRequirementList rs result

-- Eval requirements 1 by 1
evalRequirement :: Requirement -> File -> IO File
evalRequirement (Table name clms) currentFile = do
    file <- fileReadCsv name   -- this is a list of rows
    let vars = evalColumnList clms
        newTable = map (zip vars) file
    if null currentFile
        then return newTable
        else return (conjunctTable currentFile newTable)

-- calls to checkIf function to evaluate all of if requirements (rle) for each row 
-- calls to completeIf function to evaluate then and else (if true it evaluates rlt) (if false it evaluates rlf)
-- then after the evaluation the function returns the row (either full of values or empty)
evalRequirement (IfTF rle rlt rlf) currentFile = do
    let result = (completeIf rle rlt rlf currentFile)
    return result

evalRequirement (IfT rle rlt) currentFile = do
    let result = (completeIf rle rlt [] currentFile)
    return result

evalRequirement (AsignVarStr v1 s1) currentFile = do
    return (map (setVarStr v1 s1) currentFile)

evalRequirement (AsignVarVar v1 v2) currentFile = do
    return (map (setVarVar v1 v2) currentFile)

-- filter all in the currentFile ... works for 
evalRequirement req currentFile = do
    return (filter (checkRequirement req) currentFile)

-- do conjunction here
conjunctTable ::  File -> File -> File
conjunctTable [] file2 = []
conjunctTable file1 [] = file1
conjunctTable (row:rows) file2 = map (row ++) file2 ++ conjunctTable rows file2

-- This function takes the if(requirements) applyIfTrue applyIfFalse and a File
-- and returns the File after applying the if statement on every row
completeIf :: [Requirement] -> [Requirement] -> [Requirement] -> File -> File
completeIf _ _ _ [] = []
completeIf reqEval reqTrue reqFalse (row:rows)
    | checkedReqs && transformedTrue /= [] = transformedTrue : (completeIf reqEval reqTrue reqFalse rows)
    | not checkedReqs && transformedFalse /= [] = transformedFalse : (completeIf reqEval reqTrue reqFalse rows)
    | otherwise = completeIf reqEval reqTrue reqFalse rows
        where  checkedReqs = checkIf reqEval row
               transformedTrue = applyIf reqTrue row
               transformedFalse = applyIf reqFalse row

-- goes over all requirements in the if (requirements) and checks whether they are true or false
checkIf :: [Requirement] -> Row -> Bool
checkIf [] _ = True
checkIf (req:rs) row = (checkRequirement req row) && checkIf rs row

-- goes over the if application if (requirements) applyIfTrue applyIfFalse
--checkrequrement za ko e tuka
applyIf :: [Requirement] -> Row -> Row
applyIf [] row = row
applyIf ((Eq v1 v2):reqs) row
    | checkRequirement (Eq v1 v2) row = applyIf reqs row
    | otherwise = []
applyIf ((NEq v1 v2):reqs) row
    | checkRequirement (NEq v1 v2) row = applyIf reqs row
    | otherwise = []
applyIf ((EqConst v1 v2):reqs) row
    | checkRequirement (EqConst v1 v2) row = applyIf reqs row
    | otherwise = []
applyIf ((NEqConst v1 v2):reqs) row
    | checkRequirement (NEqConst v1 v2) row = applyIf reqs row
    | otherwise = []
applyIf ((IfTF rle rlt rlf):reqs) row
    | checkIf rle row = applyIf reqs (applyIf rlt row)
    | otherwise = applyIf reqs (applyIf rlf row)
applyIf ((IfT rle rlt):reqs) row
    | checkIf rle row = applyIf reqs (applyIf rlt row)
    | otherwise = row

-- Changes the value of v1 to be the value of v2
applyIf ((AsignVarVar v1 v2):reqs) row = applyIf reqs (setVarVar v1 v2 row)
applyIf ((AsignVarStr v1 v2):reqs) row = applyIf reqs (setVarStr v1 v2 row)
applyIf _ row = error "faulty input in If statement :("

-- function to fetch the value of a variable
--          var
fetchVar :: String -> Row -> String
fetchVar varName [] = ""
fetchVar varName ((var,val):restOfRow)
    | varName == var = val
    | otherwise = fetchVar varName restOfRow

--           var         
setVarStr :: String -> String -> Row -> Row
setVarStr _ _ [] = []
setVarStr changeVar newVal ((var,val):rows)
    | var == changeVar = (var,removeQuotes newVal) : setVarStr changeVar newVal rows
    | otherwise = (var,val) : setVarStr changeVar newVal rows

--           var        var
setVarVar :: String -> String -> Row -> Row
setVarVar _ _ [] = []
setVarVar v1 v2 row = setVarStr v1 ("\"" ++ fetchVar v2 row ++ "\"") row

checkRequirement :: Requirement -> Row -> Bool
checkRequirement (Eq v1 v2) row = (fetchVar v1 row) == (fetchVar v2 row)
checkRequirement (NEq v1 v2) row = (fetchVar v1 row) /= (fetchVar v2 row)
checkRequirement (EqConst v1 v2) row = (fetchVar v1 row) == removeQuotes v2
checkRequirement (NEqConst v1 v2) row = (fetchVar v1 row) /= removeQuotes v2
checkRequirement _ row = True

showResult :: [String] -> File -> String
showResult [] _ = error "There are no bound vars dummy!"
showResult _ [] = ""
showResult vars outputFile = concat (sort (map (showResultLine vars) outputFile))

showResultLine :: [String] -> Row -> String
showResultLine [var] row = fetchVar var row ++ "\n"
showResultLine (var:vars) row = fetchVar var row ++ "," ++ showResultLine vars row