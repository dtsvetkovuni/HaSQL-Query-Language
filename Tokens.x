{ 
module Tokens where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
  $white+       ; 
  "--".*        ; 
  show          { \p s -> TokenShow p } 
  where         { \p s -> TokenWhere p }
  empty         { \p s -> TokenEmpty p }
  define        { \p s -> TokenDefine p }
  for           { \p s -> TokenFor p }
  \&            { \p s -> TokenAnd p }
  \=            { \p s -> TokenEq p }
  !=          { \p s -> TokenNEq p }
  \(            { \p s -> TokenLParen p }
  \)            { \p s -> TokenRParen p }
  $digit+       { \p s -> TokenInt p (read s) } 
  $alpha [$alpha $digit \_ \’]*   { \p s -> TokenVar p s } 
 -- [$alpha $digit \_] [$alpha $digit \’]* { \p s -> TokenLocalVar p s }

{ 
-- Each action has type :: AlexPosn -> String -> Token 

-- The token type: 
data Token = 
  TokenShow AlexPosn          | 
  TokenWhere  AlexPosn        | 
  TokenEmpty AlexPosn         |
  TokenDefine  AlexPosn       |
  TokenFor  AlexPosn          |
  TokenAnd AlexPosn           |
  TokenEq AlexPosn            |
  TokenNEq AlexPosn           |
  TokenLParen AlexPosn        |
  TokenRParen AlexPosn        |
  TokenVar AlexPosn String    |
  TokenInt AlexPosn Int       
--  TokenLocalVar AlexPosn String
  deriving (Eq,Show) 

tokenPosn :: Token -> String
tokenPosn (TokenInt  (AlexPn a l c) n) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar  (AlexPn a l c) x) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenShow (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhere  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEmpty  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDefine (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFor (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
-- tokenPosn (TokenLocalVar (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

}