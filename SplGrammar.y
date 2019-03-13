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

%nonassoc '{' '}'
%nonassoc '[' ']'
%nonassoc '(' ')' 
%nonassoc '.' ','
%nonassoc while
%nonassoc if
%nonassoc then
%nonassoc else
%nonassoc print
%nonassoc '='
%left '<' '=='
%left '+' '-'
%left '*' '/'
%nonassoc length
%nonassoc stream
%nonassoc int true false var intList


%% 
Exp : int                                       { SplInt $1 }
     | var                                       { SplVar $1 }
     | true                                      { SplTrue }
     | false                                     { SplFalse }
     | Exp '==' Exp                            { SplIsEqual $1 $3 }
     | Exp '<' Exp                            { SplLessThan $1 $3 }
     | Exp '+' Exp                             { SplAdd $1 $3 }
     | Exp '-' Exp                           { SplSubtract $1 $3 }
     | Exp '*' Exp                            { SplMulti $1 $3 }
     | Exp '/' Exp                            { SplDivide $1 $3 }
     | '(' Exp ')'                               { $2 }
     | stream                                    { SplStream }
     | Exp ';' Exp                             { SplConnecting $1 $3 }
     | Type var '=' Exp                         { SplDeclare $1 $2 $3 }
     | var '=' Exp                              { SplAssignment $1 $3 }
     | while '(' Exp ')' '{' Exp '}'           { SplWhile $3 $6 }
     | print '(' Exp ')'                        { SplPrint $3 }
     | intList '[' int ']'                       { SplIntListgetElement $1 $3 }  
     | IntList var '=' Exp                      { SplIntListAssignment $2 $4 }
     | IntList var '=' '{' ListContent '}'       { SplIntListDeclare $2 $5 }
     | IntMatrix var '=' Exp                    { SplIntMatrix $2 $4 }
     | intList '.' length                        { SplGetLength $1 }

Type : Bool            { SplTypeBool } 
     | Int             { SplTypeInt } 
     | IntMatrix       { SplTypeIntMatrix }
     | IntList         { SplTypeIntList }
     
ListContent  : Int                     { SplNum $1}          
             | Int ',' ListContent     { SplContinuousNum $1 $3 }


{
parseError :: [SplToken] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data SplType = SplTypeInt | SplTypeBool | SplTypeIntMatrix | SplTypeIntList
    deriving (Show,Eq)

data SplListContent = SplNum Int | SplContinuousNum Int SplListContent
    deriving (Show,Eq)

data Expr = SplInt Int | SplVar String | SplTrue | SplFalse | SplIsEqual Expr Expr
    | SplLessThan Expr Expr | SplAdd Expr Expr | SplSubtract Expr Expr | SplMulti Expr Expr
    | SplDivide Expr Expr | SplStream | SplConnecting Expr Expr | SplDeclare SplType String Expr
    | SplAssignment String Expr | SplWhile Expr Expr | SplPrint Expr | SplIntListgetElement SplTypeIntList Int
    | SplIntListAssignment String Expr | SplIntListDeclare String SplListContent | SplIntMatrix String Expr
    | SplGetLength SplTypeIntList
    deriving (Show,Eq)
}