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
    else   { TokenElse _ }
    while  { TokenWhile _ }
    length { TokenLength _ }
    push   { TokenPush _ }
    pop    { TokenPop _ }
    getElement { TokenGetElement _}
    streams{ TokenStream _ }
    return { TokenReturn _ }
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
Sentence :: { Sentence }
Sentence :  Int var '=' Exp                                    { SplIntDeclare $2 $4 }
     | Bool var '=' Exp                                  { SplBoolDeclare $2 $4}
     | IntList var                                          { SplIntListDeclare $2}
     | IntMatrix var                                        { SplIntMatrixDeclare $2}
     | var '=' Exp                                          { SplAssignment $1 $3 }
     | return '(' var ')'                                    { SplReturn $3 }
     | IntList var '=' Exp                                  { SplIntListAssignment $2 $4 }
     | IntMatrix var '=' streams                                { SplIntMatrix $2 }                  
     | while '(' Exp ')' '{' nextLine Sentence nextLine '}'    { SplWhile $3 $7 }
     | if '(' Exp ')' '{' nextLine Sentence nextLine '}' else '{' nextLine Sentence nextLine '}'  { SplIfThenElse $3 $7 $13 }
     | if '(' Exp ')' '{' nextLine Sentence nextLine '}'                   { SplIfThen $3 $7 } 
     | Sentence nextLine Sentence                                { SplConnecting $1 $3}
     | var push  '(' Exp ')'                                    { SplIntListPush $1  $4}
     | var pop '(' ')'                                      { SplIntListPop $1 }
     
Exp :: {Expr}
Exp : int                                       { SplInt $1 }
        | var                                        { SplVar $1 }
        | true                                      { SplTrue }
        | false                                     { SplFalse }
        | streams                                    { SplStream }
        | Exp '==' Exp                            { SplIsEqual $1 $3 }
        | Exp '<' Exp                            { SplLessThan $1 $3 }
        | Exp '+' Exp                             { SplAdd $1 $3 }
        | Exp '-' Exp                           { SplSubtract $1 $3 }
        | Exp '*' Exp                            { SplMulti $1 $3 }
        | Exp '/' Exp                            { SplDivide $1 $3 }
        | '(' Exp ')'                              { $2 }     
        | var length                               { SplIntListLength $1 }
        | var getElement '(' Exp ')'               { SplIntListgetElement $1 $4}
        | intList                                   { SplIntList $1} 

{
parseError :: [SplToken] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Sentence =  SplIntDeclare String Expr | SplBoolDeclare String Expr
    | SplIntListDeclare String | SplIntMatrixDeclare String 
    | SplAssignment String Expr | SplReturn String | SplIntListAssignment String Expr 
    | SplIntMatrix String | SplWhile Expr Sentence | SplIfThenElse Expr Sentence Sentence
    | SplIfThen Expr Sentence | SplConnecting Sentence Sentence 
    | SplIntListPush String Expr | SplIntListPop String | SplEnd
    deriving (Show,Eq)

data Expr = SplInt Int | SplVar String | SplTrue | SplFalse | SplIsEqual Expr Expr
    | SplLessThan Expr Expr | SplAdd Expr Expr | SplSubtract Expr Expr | SplMulti Expr Expr
    | SplDivide Expr Expr | SplStream | SplIntListLength String | SplIntListgetElement String Expr
    | SplIntList String
    deriving (Show,Eq)
}
