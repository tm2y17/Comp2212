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
    var    { TokenVar _ $$ }

%nonassoc nextLine
%nonassoc while
%nonassoc '{' '}'
%left ';' 
%nonassoc '(' ')' 
%nonassoc '[' ']'
%nonassoc ','
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
     | Int var '=' Exp                         { SplIntDeclare $2 $4 }
     | Bool var '=' Exp                         { SplBoolDeclare $2 $4}
     | var '=' Exp                              { SplAssignment $1 $3 }
     | while '(' Exp ')' '{' nextLine Exp nextLine '}'   { SplWhile $3 $7 }
     | print '(' Exp ')'                        { SplPrint $3 }
     | intList '[' int ']'                       { SplIntListgetElement $1 $3 }  
     | IntList var '=' Exp                      { SplIntListAssignment $2 $4 }
     | IntMatrix var '=' Exp                   { SplIntMatrix $2 }
     | intList '.' length                        { SplGetLength $1 }
     | if Exp nextLine then Exp nextLine else Exp    { SplIfThenElse $2 $5 $8 } 
     | if Exp nextLine then Exp                      { SplIfThen $2 $5 } 
     | Exp ';' Exp                               { SplConnecting $1 $3}
     | Exp ';'                                   { SplSentence $1}



{
parseError :: [SplToken] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data SplType = SplTypeInt | SplTypeBool | SplTypeIntMatrix | SplTypeIntList
    deriving (Show,Eq)


data Expr = SplInt Int | SplVar String | SplTrue | SplFalse | SplIsEqual Expr Expr
    | SplLessThan Expr Expr | SplAdd Expr Expr | SplSubtract Expr Expr | SplMulti Expr Expr
    | SplDivide Expr Expr | SplStream | SplIntDeclare String Expr | SplBoolDeclare String Expr
    | SplAssignment String Expr | SplWhile Expr Expr | SplPrint Expr | SplIntListgetElement String Int
    | SplIntListAssignment String Expr | SplIntMatrix String
    | SplGetLength String | SplIfThenElse Expr Expr Expr | SplIfThen Expr Expr | SplConnecting Expr Expr
    | SplSentence Expr
    deriving (Show,Eq)
}



















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

%nonassoc nextLine
%nonassoc while
%nonassoc '{' '}'
%left ';' 
%nonassoc '(' ')' 
%nonassoc '[' ']'
%nonassoc ','
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
Sentence :  while '(' Exp ')' '{' nextLine Sentence nextLine '}'         { SplWhile $3 $7 }
        | while '(' Exp ')' '{' nextLine Sentence nextLine '}' nextLine  { SplWhile $3 $7 }
        | if Exp nextLine then Sentence nextLine else Sentence           { SplIfThenElse $2 $5 $8 }
        | if Exp nextLine then Sentence nextLine else Sentence nextLine  { SplIfThenElse $2 $5 $8 }
        | if Exp nextLine then Sentence                                  { SplIfThen $2 $5 } 
        | if Exp nextLine then Sentence nextLine                         { SplIfThen $2 $5 } 
        | while '(' Exp ')' '{' nextLine Exp nextLine '}'                { SplWhile' $3 $7 }
        | while '(' Exp ')' '{' nextLine Exp nextLine '}' nextLine       { SplWhile' $3 $7 }
        | Sentence nextLine Sentence                                     { SplConnecting $1 $3}
        | Sentence nextLine Exp                                          { SplConnecting' $1 $3}
        | Exp nextLine Sentence                                          { SplConnecting'' $1 $3}
        | Exp nextLine                                                   { SplConnecting''' $1}
        | Sentence nextLine                                              { SplConnecting'''' $1}

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
     | Int var '=' Exp nextLine                      { SplIntDeclare $2 $4 }
     | Bool var '=' Exp                              { SplBoolDeclare $2 $4}
     | Bool var '=' Exp nextLine                         { SplBoolDeclare $2 $4}
     | var '=' Exp                                      { SplAssignment $1 $3 }
     | var '=' Exp nextLine                              { SplAssignment $1 $3 }
     | print '(' Exp ')'                                 { SplPrint $3 }
     | print '(' Exp ')' nextLine                        { SplPrint $3 }
     | IntList var '=' Exp                      { SplIntListAssignment $2 $4 }
     | IntList var '=' Exp nextLine                      { SplIntListAssignment $2 $4 }
     | IntMatrix var '=' Exp                   { SplIntMatrix $2 }
     | IntMatrix var '=' Exp nextLine                   { SplIntMatrix $2 }
     | var length                                       { SplIntListLength $1 }
     | var push  '(' int ')'                            { SplIntListPush $1  $4}
     | var pop '(' int ')'                                 { SplIntListPop $1 $4}
     | var getElement '(' int ')'               { SplIntListgetElement $1 $4} 
{
parseError :: [SplToken] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Sentence = SplWhile Expr Sentence
        | SplIfThenElse Expr Sentence Sentence
        | SplIfThen Expr Sentence
        | SplWhile' Expr Expr
        | SplConnecting Sentence Sentence
        | SplConnecting' Sentence Expr
        | SplConnecting'' Expr Sentence
        | SplConnecting''' Expr
        | SplConnecting'''' Sentence
         deriving (Show,Eq)

data Expr = SplInt Int | SplVar String | SplTrue | SplFalse | SplIsEqual Expr Expr
    | SplLessThan Expr Expr | SplAdd Expr Expr | SplSubtract Expr Expr | SplMulti Expr Expr
    | SplDivide Expr Expr | SplStream | SplIntDeclare String Expr | SplBoolDeclare String Expr
    | SplAssignment String Expr | SplPrint Expr | SplIntListAssignment String Expr 
    | SplIntMatrix String | SplIntListLength String | SplIntListPush String Int| SplIntListPop String Int 
    | SplIntListgetElement String Int 
    deriving (Show,Eq)
}

