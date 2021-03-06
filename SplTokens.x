{ 
module SplTokens where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters
$white = [\ \t\f\v\r]

tokens :-    
  \n         { tok (\p s -> TokenNextLine p) }
  $white+       ;
  "--".*         { tok (\p s -> TokenComments p)}
  Bool           { tok (\p s -> TokenTypeBool p)} 
  Int\[\]\[\]    { tok (\p s -> TokenTypeIntMatrix p) }
  Int\[\]        { tok (\p s -> TokenTypeIntList p) }
  Int            { tok (\p s -> TokenTypeInt p) }
  \[ [$digit \,]* $digit \]   { tok (\p s -> TokenIntList p s) } 
  $digit+        { tok (\p s -> TokenInt p (read s)) }
  true           { tok (\p s -> TokenTrue p) }
  false          { tok (\p s -> TokenFalse p) }
  \<             { tok (\p s -> TokenLessThan p) }
  \+             { tok (\p s -> TokenPlus p) }
  \-             { tok (\p s -> TokenSubtract  p) }
  \*             { tok (\p s -> TokenMulti p) }
  \/             { tok (\p s -> TokenDivide p) }
  if             { tok (\p s -> TokenIf p) }
  else           { tok (\p s -> TokenElse p) }
  while          { tok (\p s -> TokenWhile p) }
  \.length       { tok (\p s -> TokenLength p) }
  \.push         { tok (\p s -> TokenPush p) }
  \.pop          { tok (\p s -> TokenPop p) }
  \.getElement   { tok (\p s -> TokenGetElement p) }
  streams        { tok (\p s -> TokenStream p) }
  return         { tok (\p s -> TokenReturn p) }
  =              { tok (\p s -> TokenEq p )}
  ==             { tok (\p s -> TokenCompare p )}
  \(             { tok (\p s -> TokenLParen p) }
  \)             { tok (\p s -> TokenRParen p) }
  \[             { tok (\p s -> TokenLBracket p) }
  \]             { tok (\p s -> TokenRBracket p) }
  \{             { tok (\p s -> TokenLBigParen p) }
  \}             { tok (\p s -> TokenRBigParen p) }
  \;             { tok (\p s -> TokenSemicolon p) }

  $alpha [$alpha $digit \_ \’]*   { tok (\p s -> TokenVar p s) } 
{ 
-- Each action has type :: AlexPosn -> String -> MDLToken 

-- Helper function
tok f p s = f p s

-- The token type: 
data SplToken = 
  TokenNextLine AlexPosn         |
  TokenComments AlexPosn         |
  TokenTypeBool AlexPosn         | 
  TokenTypeInt  AlexPosn         | 
  TokenInt AlexPosn Int          |
  TokenTypeIntList AlexPosn      | 
  TokenTypeIntMatrix AlexPosn    |
  TokenIntList AlexPosn String   |
  TokenTrue AlexPosn             |
  TokenFalse AlexPosn            |
  TokenLessThan AlexPosn         |
  TokenPlus AlexPosn             |
  TokenSubtract AlexPosn         |
  TokenMulti AlexPosn            |
  TokenDivide AlexPosn           |
  TokenIf AlexPosn               |
  TokenElse AlexPosn             |
  TokenWhile AlexPosn            |
  TokenLength AlexPosn           |
  TokenPush AlexPosn             |
  TokenPop  AlexPosn             |
  TokenGetElement  AlexPosn      |
  TokenStream AlexPosn           |
  TokenReturn AlexPosn           |
  TokenEq AlexPosn               |
  TokenCompare AlexPosn          |
  TokenLParen AlexPosn           |
  TokenRParen AlexPosn           |
  TokenLBracket AlexPosn         |
  TokenRBracket AlexPosn         |
  TokenLBigParen AlexPosn        |
  TokenRBigParen AlexPosn        |
  TokenSemicolon AlexPosn        |
  TokenVar AlexPosn String
  deriving (Eq,Show) 

tokenPosn :: SplToken -> String
tokenPosn (TokenNextLine (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenComments (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeBool (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeInt  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInt  (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeIntList  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeIntMatrix (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIntList (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTrue  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFalse  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessThan  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSubtract (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMulti  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDivide (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLength (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPush (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPop (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGetElement (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenStream (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReturn (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEq  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCompare  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLBracket (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRBracket (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLBigParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRBigParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSemicolon (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)

}
