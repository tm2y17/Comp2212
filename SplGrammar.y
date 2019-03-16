{ 
module SplGrammar where 
import SplTokens 
}

%name parseCalc 
%tokentype { SplToken } 
%error { parseError }
%token 
    nextLine  { TokenNextLine _ }
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
    push   { TokenPush _ }
    pop    { TokenPop _ }
    getElement { TokenGetElement _}
    streams{ TokenStream _ }
    print  { TokenPrint _ }
    '='    { TokenEq _ }
    '=='   { TokenCompare _ }
    '('    { TokenLParen _ } 
    ')'    { TokenRParen _ } 
    '{'    { TokenLBigParen _ }
    '}'    { TokenRBigParen _ }
    var    { TokenVar _ $$ }

%left nextLine
%nonassoc while
%nonassoc '{' '}'
%nonassoc '(' ')' 
%nonassoc '[' ']'
%nonassoc if
%nonassoc then
%nonassoc else
%nonassoc print
%nonassoc '='
%left '<' '=='
%left '+' '-'
%left '*' '/'
%nonassoc '.' length
%nonassoc stream
%nonassoc int true false var intList


%% 
Exp :: { Expr }
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
     | streams                                    { SplStream }
     | Int var '=' Exp                               { SplIntDeclare $2 $4 }
     | Bool var '=' Exp                              { SplBoolDeclare $2 $4}
     | Exp '=' Exp                                      { SplAssignment $1 $3 }
     | print '(' Exp ')'                                 { SplPrint $3 }
     | IntList var '=' Exp                      { SplIntListAssignment $2 $4 }
     | IntMatrix var '=' Exp                   { SplIntMatrix $2 }
     | var length                                       { SplIntListLength $1 }
     | var push  '(' Exp ')'                            { SplIntListPush $1  $4}
     | var pop '(' Exp ')'                                 { SplIntListPop $1 $4}
     | var getElement '(' Exp ')'                           { SplIntListgetElement $1 $4} 
     | while '(' Exp ')' '{' nextLine Exp nextLine '}'       { SplWhile $3 $7 }
     | if Exp nextLine then Exp nextLine else Exp              { SplIfThenElse $2 $5 $8 }
     | if Exp nextLine then Exp                                { SplIfThen $2 $5 } 
     | Exp nextLine Exp                                         { Connecting $1 $3}
     | Exp nextLine                                              {Sentence $1}
     
{
parseError :: [SplToken] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))


data Expr = SplInt Int | SplVar String | SplTrue | SplFalse | SplIsEqual Expr Expr
    | SplLessThan Expr Expr | SplAdd Expr Expr | SplSubtract Expr Expr | SplMulti Expr Expr
    | SplDivide Expr Expr | SplStream | SplIntDeclare String Expr | SplBoolDeclare String Expr
    | SplAssignment Expr Expr | SplPrint Expr | SplIntListAssignment String Expr 
    | SplIntMatrix String | SplIntListLength String | SplIntListPush String Expr| SplIntListPop String Expr 
    | SplIntListgetElement String Expr | SplWhile Expr Expr | SplIfThenElse Expr Expr Expr
    | SplIfThen Expr Expr | Connecting Expr Expr | Sentence Expr
    deriving (Show,Eq)
}
