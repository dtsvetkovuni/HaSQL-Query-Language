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
  use           { \p s -> TokenUse p }
  from          { \p s -> TokenFrom p }
  \&            { \p s -> TokenAnd p }
  \=            { \p s -> TokenEq p }
  !=            { \p s -> TokenNEq p }
  \(            { \p s -> TokenLParen p }
  \)            { \p s -> TokenRParen p }
  \,            { \p s -> TokenSeparator p }
  $digit+       { \p s -> TokenInt p (read s) } 
  $alpha [$alpha $digit \_ \’]*            { \p s -> TokenVar p s } 
  [$alpha $digit \_ \’]+                   { \p s -> TokenString p s }
 -- [$alpha $digit \_] [$alpha $digit \’]* { \p s -> TokenLocalVar p s }

{ 
-- Each action has type :: AlexPosn -> String -> Token 

-- The token type: 
data Token = 
  TokenShow AlexPosn          | 
  TokenWhere  AlexPosn        | 
  TokenEmpty AlexPosn         |
  TokenUse  AlexPosn          |
  TokenFrom  AlexPosn         |
  TokenAnd AlexPosn           |
  TokenEq AlexPosn            |
  TokenNEq AlexPosn           |
  TokenLParen AlexPosn        |
  TokenRParen AlexPosn        |
  TokenSeparator AlexPosn     |
  TokenVar AlexPosn String    |
  TokenString AlexPosn String |
  TokenInt AlexPosn Int       
--  TokenLocalVar AlexPosn String
  deriving (Eq,Show) 

tokenPosn :: Token -> String
tokenPosn (TokenInt  (AlexPn a l c) n) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar  (AlexPn a l c) x) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenString  (AlexPn a l c) x) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenShow (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhere  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEmpty  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenUse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFrom (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSeparator (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
-- tokenPosn (TokenLocalVar (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

}