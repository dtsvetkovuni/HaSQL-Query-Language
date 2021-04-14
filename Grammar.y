{ 
module Grammar where 
import Lexer 
}

%name calc 
%tokentype { Token } 
%error { parseError }
%token 
    show   { TokenShow _ } 
    where  { TokenWhere _ } 
    empty  { TokenEmpty _ } 
    '&'    { TokenAnd _ }
    '_'    { TokenSkip _ }
    '='    { TokenEq _ }
    "!="   { TokenNEq _ }
    '('    { TokenLParen _ } 
    ')'    { TokenRParen _ } 
    ','    { TokenSeparator _ }
--    '"'    { TokenQuote _ }
    var    { TokenVar _ $$ }
    str    { TokenString _ $$ }

%right for 
%left '&' 

--haskell happy documentation 2.5. Monadic Parsers
%monad { E } { thenE } { returnE }

%% 

Exp : show '(' BoundVars ')' where RequirementList  { FinalExp  $3 $6 }

BoundVars : var ',' BoundVars    { $1:$3 }
     | var                       { [$1] }

--Vars : var                   { Variable $1}
--     | '"' str '"'           { Constants $2 }

ColumnList : Column ',' ColumnList     { $1:$3 }
     | Column                          { [$1] }

Column : var                   { Var $1 }
     | '_'                     { SkipVar }

RequirementList : Requirement '&' RequirementList { $1:$3 }
     | Requirement { [$1] }

Requirement : str '(' ColumnList ')'           { Table $1 $3 }
     | var '=' var                             { Eq $1 $3 }
     | var "!=" var                            { NEq $1 $3 } 
     | var '=' empty                           { Empty $1 }
     | var "!=" empty                          { NotEmpty $1 }
--     | var '=' '"' str '"'                     { EqConst $1 $4 }
--     | var "!=" '"' str '"'                    { NEqConst $1 $4 }

{ 

---------------------DATA TYPES---------------------------

data Exp = FinalExp ColumnList RequirementList
     deriving Show

data Column = Var String 
      | SkipVar
      deriving Show

data Requirement = Table String ColumnList
      | Eq String String
      | NEq String String
      | Empty String
      | NotEmpty String
     deriving Show      

type RequirementList = [Requirement]
type ColumnList = [Column]



--------------------error handling from 2.5.1----------------

data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = 
   case m of 
       Ok a -> k a
	 Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = 
   case m of
      Ok a -> OK a
	Failed e -> k e

parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))
parseError tokens = failE "Parse error"

} 