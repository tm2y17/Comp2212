{ 
module SplGrammar where 
import SplTokens 
}

%name parseCalc 
%tokentype { SplToken } 
%error { parseError }
%token 
    Bool      { TokenTypeBool _ } 
    IntMatrix { TokenTypeIntMatrix _ }
    IntList   { TokenTypeIntList _ }
    Int       { TokenTypeInt _ }  
    intList   { TokenIntList _ $$ }
    int       { TokenInt _ $$ } 
    true      { TokenTrue _ }
    false  { TokenFalse _ }
    '<'    { TokenLessThan _ }
    '+'    { TokenPlus _ }
    '-'    { TokenSubtract _ }
    '*'    { TokenMulti _ }
    '/'    { TokenDivide _ }
    if     { TokenIf _ }
    then   { TokenThen _ }
    else   { TokenElse _ }
    while  { TokenWhile _ }
    length { TokenLength _ }
    stream { TokenStream _ }
    print  { TokenPrint _ }
    '='    { TokenEq _ }
    '=='   { TokenCompare _ }
    '('    { TokenLParen _ } 
    ')'    { TokenRParen _ } 
    '['    { TokenLBracket _ }
    ']'    { TokenRBracket _ } 
    '{'    { TokenLBigParen _ }
    '}'    { TokenRBigParen _ }
    ';'    { TokenSemicolon _ }
    '.'    { TokenDot _ }
    ','    { TokenComma _ }
    var    { TokenVar _ $$ }
