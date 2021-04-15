{ 
module Grammar where 
import Lexer 
}

%name queryLang 
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
--%monad { E } { thenE } { returnE }

%% 

Exp : show '(' BoundVars ')' where RequirementList  { FinalExp  $3 $6 }

BoundVars : var ',' BoundVars    { $1:$3 }
     | var                       { [$1] }

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
--     | var '=' str                     { EqConst $1 $4 }
--     | var "!=" str                    { NEqConst $1 $4 }

--Vars : var                   { Variable $1}
--     | '"' str '"'           { Constants $2 }

{ 

---------------------DATA TYPES---------------------------

data Exp = FinalExp [String] RequirementList
     deriving (Eq,Show) 

data Column = Var String
      | SkipVar
     deriving (Eq,Show) 

data Requirement = Table String ColumnList
      | Eq String String
      | NEq String String
      | Empty String
      | NotEmpty String
     deriving (Eq,Show)      

type RequirementList = [Requirement]
type ColumnList = [Column]



parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))
parseError tokens = error "Parse error"

} 