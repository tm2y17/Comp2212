module SplEval where
    import SplGrammar
    
    --data Sentence =  SplIntDeclare String Expr | SplBoolDeclare String Expr
    --    | SplAssignment Expr Expr | SplPrint Expr | SplIntListAssignment String Expr 
    --    | SplIntMatrix String | SplWhile Expr Sentence | SplIfThenElse Expr Sentence Sentence
    --    | SplIfThen Expr Sentence | SplConnecting Sentence Sentence | SplSentence Sentence
    --    | SplIntListPush String Expr | SplIntListPop String Expr
    --    deriving (Show,Eq)
    
    --data Expr = SplInt Int | SplVar String | SplTrue | SplFalse | SplIsEqual Expr Expr
    --    | SplLessThan Expr Expr | SplAdd Expr Expr | SplSubtract Expr Expr | SplMulti Expr Expr
    --    | SplDivide Expr Expr | SplStream | SplIntListLength String | SplIntListgetElement String Expr
    --    | SplIntList String
    --    deriving (Show,Eq)
    
    type Environment = [(String, Expr)]
    type State = (Sentence,Sentence,Environment)
    
    data Memory = DoPrint Expr
    
    {-
    evalDeclare :: Expr -> (String,BasicType)
    evalDeclare (SplIntDeclare name expr) 
        | isTInt (eval expr) = (name, eval expr)
        | otherwise = error "not int"
    
    evalDeclare (SplBoolDeclare name expr) 
        | isTBool (eval expr) = (name, eval expr)
        | otherwise = error "not bool"
    
    isTInt :: BasicType -> Bool
    isTInt (TInt x) = True
    isTInt x = False
    
    isTBool :: BasicType -> Bool
    isTBool (TBool x) = True
    isTBool x = False
    -}
    
    
    formatSentence :: Sentence -> [Sentence]
    formatSentence (SplConnecting x1 x2) = formatSentence x1 ++ formatSentence x2
    formatSentence (SplSentence x) = [x] 
    formatSentence x = [x]
    
    
    --evalLoop :: Sentence -> Memory 
    --evalLoop (SplPrint (SplTrue)) = 
    