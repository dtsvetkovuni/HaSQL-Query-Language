{ 
module Grammar where 
import Lexer 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token 
    show   { TokenShow _ } 
    where  { TokenWhere _ } 
    empty  { TokenEmpty _ } 
    use    { TokenUse _ }
    from   { TokenFrom _ }
    '&'    { TokenAnd _ }
    '_'    { TokenSkip _ }
    '='    { TokenEq _ }
    "!="   { TokenNEq _ }
    '('    { TokenLParen _ } 
    ')'    { TokenRParen _ } 
    ','    { TokenSeparator _ }
    '"'    { TokenQuote _ }
    var    { TokenVar _ $$}
    str    { TokenString _ $$ }
 --   localVar  { TokenLocalVar _ $$}

%right for 
%left '&' 
%% 

Exp : show '(' BoundVars ')' where RequirementsList  { FinalExp  $3 $6 }

BoundVars : var ',' BoundVars    { $1:$3 }
     | var                       { [$1] }

--Vars : var                   { Variable $1}
--     | '"' str '"'           { Constants $2 }

ColumnsList : Column ',' ColumnsList { $1:$3 }
     | Column                        { [$1] }

Column : var                   { Var $1 }
     | '_'                     { SkipVar }

RequirementsList : RequirementsList '&' RequirementsList { $1:$3 }
     | Requirement { [$1] }

Requirement : str '(' Column ')'                 { Table $1 $3 }
     | var '=' var                             { Eq $1 $3 }
     | var "!=" var                            { NEq $1 $3 } 
     | var '=' empty                           { Empty $1 }
     | var "!=" empty                          { NotEmpty $1 }
--     | var '=' '"' str '"'                     { EqConst $1 $4 }
--     | var "!=" '"' str '"'                    { NEqConst $1 $4 }

{ 
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Exp = FinalExp Column RequirementsList
     deriving Show


data Columnls = Var String 
      | ListColumn String Column

data Requirement = And Requirement RequirementsList
      | Eq String String
      | NEq String String
      | Empty String
      | NotEmpty String
     deriving Show      

data RequirementsList = [Requirement]
} 