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
    define { TokenDefine _ }
    for    { TokenFor _ }
    '&'     { TokenAnd _ }
    '='     { TokenEq _ }
    "!="   { TokenNEq _ }
    '('    { TokenLParen _ } 
    ')'    { TokenRParen _ } 
    var    { TokenVar _ $$}
 --   localVar  { TokenLocalVar _ $$}

%right for 
%left '&' 
%% 

Exp : show Cols where Requirements { FinalExp  $2 $4 } 
Cols : var           { Var $1 }
     | var Cols      { ListCols $1 $2 }   
Requirements : Requirements '&' Requirements   { And $1 $3 }
     | var '=' var                             { Eq $1 $3 }
     | var "!=" var                            { NEq $1 $3 } 
     | var '=' empty                            { Empty $1 }
     | var "!=" empty                            { NotEmpty $1 }
     | var '(' Cols ')'                        { Table $1 $3 }
     | define var for Requirements             { Def $2 $4 }
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
      | Table String Cols
      | Def String Requirements
     deriving Show      
} 