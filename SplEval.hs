module SplEval where
    import SplGrammar
    
    
    type Environment = [(String,String)]
    type State = (Sentence,[Sentence],Environment)
    
    deriving (Show,Eq)
    
    formatSentence :: Sentence -> [Sentence]
    formatSentence (SplConnecting x1 x2) = formatSentence x1 ++ formatSentence x2
    formatSentence x = [x]
    
    parseToState :: Sentence -> State
    parseToState s = (head (formatSentence s), tail (formatSentence s),[])
    
    getMatrix :: Sentence -> Environment -> String
    
    
    evalLoop :: State -> [[Int]] -> String
    evalLoop (current, rest, e) int_list 
        | isReturn current = getMatrix current e
        | !(isReturn current) = evalLoop (eval (current, rest, e))
    
    
    eval :: State -> State
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    eval 
    
    
    isSplInt :: Expr -> Bool
    isSplInt (SplInt x) = True
    isSplInt _ = False
    
    isSplTrue :: Expr -> Bool
    isSplTrue SplTrue = True
    isSplTrue _ = False
    
    isSplFalse :: Expr -> Bool
    isSplFalse SplFalse = True
    isSplFalse _ = False
    
    isSplIntList:: Expr -> Bool
    isSplIntList (SplIntList x) = True
    isSplIntList _ = False
    
    isReturn :: Sentence -> Bool
    isReturn (SplReturn Expr) = True
    isReturn _ = False