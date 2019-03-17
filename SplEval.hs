module SplEval where
    import SplGrammar
    
    data SplType = TyInt | TyBool | TyIntList | TyIntMatrix 
        deriving (Show,Eq)
    
    type Environment = [(String,String,SplType)]
    type State = (Sentence,[Sentence],Environment)
    
    formatSentence :: Sentence -> [Sentence]
    formatSentence (SplConnecting x1 x2) = formatSentence x1 ++ formatSentence x2
    formatSentence x = [x]
    
    parseToState :: Sentence -> [[Int]] -> State
    parseToState s int_list = (head (formatSentence s), tail (formatSentence s),[("!!!",show int_list, TyIntMatrix)])
    
    
    {-
    getMatrix :: Sentence -> Environment -> String
    
    
    evalLoop :: State -> String
    evalLoop (current, rest, e) 
        | isReturn current = getMatrix current e
        | !(isReturn current) = evalLoop (eval (current, rest, e))
    -}
    
    eval :: State -> State
    -- SplIntDeclare  Int i = 3
    eval ( (SplIntDeclare var (SplInt x) ),rest, e) 
        | checkSameVariable var e = error "Duplicate variable"
        | length rest > 0 = eval (head rest, tail rest, (var,(show x), TyInt):e )
        | otherwise = error "invalid last line"
    eval ( (SplIntDeclare var _ ),rest, e) = error "Invalid variable, Int expected"
    
    -- SplBoolDeclare  Bool x = false
    eval ( (SplBoolDeclare var SplTrue ),rest, e) 
        | checkSameVariable var e = error "Duplicate variable"
        | length rest > 0 = eval (head rest, tail rest, (var, "True", TyBool):e )
        | otherwise = error "invalid last line"
    eval ( (SplBoolDeclare var SplFalse ),rest, e) 
        | checkSameVariable var e = error "Duplicate variable"
        | length rest > 0 = eval (head rest, tail rest, (var, "False", TyBool):e )
        | otherwise = error "invalid last line"
    eval ( (SplBoolDeclare var _),rest, e) = error "Invalid variable, Bool expected"
    
    -- SplIntListDeclare1   Int[] x = [1,2,3]
    eval ( (SplIntListAssignment var (SplIntList x)) , rest, e)
        | checkSameVariable var e = error "Duplicate variable"
        | length rest > 0 = eval (head rest, tail rest, (var, (show x), TyIntList):e )
        | otherwise = error "invalid last line"
    
    -- SplIntListDeclare2   Int[] x
    eval ((SplIntListDeclare var), rest, e )
        | checkSameVariable var e = error "Duplicate variable"
        | length rest > 0 = eval (head rest, tail rest, (var, "[]", TyIntList):e )
        | otherwise = error "invalid last line"
    
    -- SplIntMatrixDeclare Int[][] x
    eval ( (SplIntMatrixDeclare var), rest, e)
        | checkSameVariable var e = error "Duplicate variable"
        | length rest > 0 = eval (head rest, tail rest, (var, "[]", TyIntMatrix):e )
        | otherwise = error "invalid last line"
    
    --SplIntMatrix  Int[][] x= streams
    eval ( (SplIntMatrix var ), rest, e )  
        | checkSameVariable var e = error "Duplicate variable"
        | length rest > 0 = eval (head rest, tail rest, (var, b, TyIntMatrix):e )
        | otherwise = error "invalid last line"
        where (a,b,c) = getVarTuple "!!!" e
    
    -- SplAssignment      var = Exp
    
    
    -- push     x.push(3)
    eval ( (SplIntListPush var (SplInt x) ), rest, e)
        | length rest > 0 && c == TyIntList = eval (head rest, tail rest, currentEnvir ++ [(a,new_int_list,c)] )
        | c /= TyIntList = error "The type is not a IntList"
        | otherwise = error "invalid last line"
        where currentEnvir = deleteEnvir var e
              (a,b,c) = getVarTuple var e
              int_list = read b :: [Int]
              new_int_list = show (int_list++[x])
    
    -- push     x.push([1,2,3])
    eval ( (SplIntListPush var (SplIntList x) ), rest, e)
        | length rest > 0 && c == TyIntMatrix = eval (head rest, tail rest, currentEnvir ++ [(a,new_int_list,c)] )
        | c /= TyIntMatrix = error "The type is not a IntMatrix"
        | otherwise = error "invalid last line"
        where currentEnvir = deleteEnvir var e
              (a,b,c) = getVarTuple var e
              int_list = read b :: [[Int]]
              new_x = read x :: [Int]
              new_int_list = show (int_list++[new_x])
    
    -- pop   x.pop()  IntList   pop   x.pop()  IntMatrix
    eval ( (SplIntListPop var), rest, e)
        | length rest > 0 && c == TyIntList && length real_b > 0 =  eval (head rest, tail rest, currentEnvir ++ [(a,new_b,c)] ) 
        | length rest > 0 && c == TyIntMatrix && length real_b' > 0 =  eval (head rest, tail rest, currentEnvir ++ [(a,new_b',c)] ) 
        | length real_b < 1 || length real_b'<1 = error "cannot pop because the list is empty"
        | c /= TyIntMatrix = error "The type is not a Intmatrix"
        | c /= TyIntList = error "The type is not a IntList"
        | length rest == 0 = error "invalid last line"
        where currentEnvir = deleteEnvir var e
              (a,b,c) = getVarTuple var e
              real_b = read b :: [Int]
              real_b' = read b :: [[Int]]
              new_b = show (init real_b)
              new_b' = show (init real_b')
    {-
    eval ((SplWhile expr sen),rest, e)
        | length rest > 0 && evalExpr expr == SplTrue = 
        | length rest > 0 && evalExpr expr == SplFalse = 
    -}
    --eval x = x
    
    
    --Less Than Exp reduction
    evalExpr :: Expr -> Expr
    evalExpr (SplLessThan (SplInt n1) (SplInt n2)) = getBool (n1 < n2) 
    evalExpr (SplLessThan e1 e2)  =  evalExpr (SplLessThan (evalExpr e1) (evalExpr e2))
    
    --Add Exp reduction
    evalExpr (SplAdd (SplInt n1) (SplInt n2)) = SplInt (n1+n2)
    evalExpr (SplAdd e1 e2) = evalExpr (SplAdd (evalExpr e1) (evalExpr e2))
    
    --Substract Exp reduction
    evalExpr (SplSubtract (SplInt n1) (SplInt n2)) = SplInt (n1-n2)
    evalExpr (SplSubtract e1 e2) = evalExpr (SplSubtract (evalExpr e1) (evalExpr e2))
    
    --Divide Exp reduction
    evalExpr (SplDivide (SplInt n1) (SplInt n2)) = SplInt (n1 `div` n2) 
    evalExpr (SplDivide e1 e2) = evalExpr (SplDivide (evalExpr e1) (evalExpr e2))
    
    evalExpr (SplInt n) = (SplInt n)
    
    
    
    
    
    
    
    
    
    
    
    getVarTuple :: String -> Environment -> (String,String,SplType)
    getVarTuple name (x:xs) 
        | name == (get1st x) = x
        | name /= (get1st x) = getVarTuple name xs
        | otherwise = error "no variable found"
    
    deleteEnvir :: String -> Environment -> Environment
    deleteEnvir name (x:xs)
        | name == (get1st x) = [] ++ deleteEnvir name xs
        | name /= (get1st x) = [x] ++ deleteEnvir name xs
    deleteEnvir _ [] = [] 
    
    checkSameVariable :: String -> Environment -> Bool
    checkSameVariable var (x:xs)
        | var == get1st x = True || checkSameVariable var xs
        | var /= get1st x = False || checkSameVariable var xs
    checkSameVariable _ [] = False
    
    getBool :: Bool -> Expr
    getBool e 
        | e == True = SplTrue
        | e == False = SplFalse
    
    get1st (a,_,_) = a
    
    get2nd (_,b,_) = b
    
    get3rd (_,_,c) = c
        
    