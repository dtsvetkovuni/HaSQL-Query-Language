{ 
module Grammar where 
import Tokens 
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
    '='    { TokenEq _ }
    "!="   { TokenNEq _ }
    '('    { TokenLParen _ } 
    ')'    { TokenRParen _ } 
    ','    { TokenSeparator _ }
    var    { TokenVar _ $$}
    str    { TokenString _ $$ }
 --   localVar  { TokenLocalVar _ $$}

%right for 
%left '&' 
%% 

Exp : show '(' BoundVars ')' from Tables where Requirements { FinalExp  $3 $7 $10 $12 } 

--vars vars everywhere
--var here var there it is vary good

BoundVars : var              { BoundVar $1 }
     | var ',' BoundVars     { BoundCols $1 $3 }
     | def

FreeVars : var               { FreeVar $1 }
     | var ',' FreeVars      { FreeCols $1 $3 }   

Cols : var                   { Var $1 }
     | var ',' Cols          { AllCols $1 $3 } 

Tables : var '(' Cols ')'           { Table $1 $3}
     | var '(' Cols ')' ',' Tables  { Tables $1 $3}

Requirements : Requirements '&' Requirements   { And $1 $3 }
     | var '=' var                             { Eq $1 $3 }
     | var "!=" var                            { NEq $1 $3 } 
     | var '=' empty                           { Empty $1 }
     | var "!=" empty                          { NotEmpty $1 }

{ 
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Exp = FinalExp Cols Requirements
     deriving Show


data Cols = Var String 
      | ListCols String Cols

data Requirements = And Requirements Requirements
      | Eq String String
      | NEq String String
      | Empty String
      | NotEmpty String
     deriving Show      
} 