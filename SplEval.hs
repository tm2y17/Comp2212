module SplEval where
    import SplGrammar
    
    data SplType = TyInt | TyBool | TyIntList | TyIntMatrix 
        deriving (Show,Eq)
    
    type Environment = [(String,String,SplType,Int)]
    type State = (Sentence,[Sentence],Environment,Int)
    
    formatSentence :: Sentence -> [Sentence]
    formatSentence (SplConnecting x1 x2) = formatSentence x1 ++ formatSentence x2
    formatSentence x = [x]
    
    parseToState :: Sentence -> [[Int]] -> State
    parseToState s int_list = (head (formatSentence s), tail (formatSentence s),[("!!!",show int_list, TyIntMatrix,0)],0)
    
    
    
    evalLoop :: State -> String
    evalLoop state =  b 
            where ( (SplReturn var ), rest , e, level) = eval state
                  (a,b,c,d) = getVarTuple var e 
    
    evalTesting :: State -> Environment
    evalTesting state = e
            where ( (SplReturn var ), rest , e, level) = eval state
    
    eval :: State -> State
    -- SplIntDeclare  Int i = 3
    eval ( (SplIntDeclare var (SplInt x) ),rest, e, level) 
        | checkSameVariable var e = error "Duplicate variable"
        | length rest > 0 = eval (head rest, tail rest, (var,(show x), TyInt, level):e, level)
        | otherwise = error "invalid last line"
    eval ( (SplIntDeclare var _ ),rest, e, level) = error "Invalid variable, Int expected"
    
    
    -- SplBoolDeclare  Bool x = false
    eval ( (SplBoolDeclare var SplTrue ),rest, e, level) 
        | checkSameVariable var e = error "Duplicate variable"
        | length rest > 0 = eval (head rest, tail rest, (var, "True", TyBool, level):e, level)
        | otherwise = error "invalid last line"
    eval ( (SplBoolDeclare var SplFalse ),rest, e, level) 
        | checkSameVariable var e = error "Duplicate variable"
        | length rest > 0 = eval (head rest, tail rest, (var, "False", TyBool, level):e, level)
        | otherwise = error "invalid last line"
    eval ( (SplBoolDeclare var _),rest, e, level) = error "Invalid variable, Bool expected"
    
    -- SplIntListDeclare1   Int[] x = [1,2,3]
    eval ( (SplIntListAssignment var (SplIntList x)) , rest, e, level)
        | checkSameVariable var e = error "Duplicate variable"
        | length rest > 0 = eval (head rest, tail rest, (var, x, TyIntList, level):e, level)
        | otherwise = error "invalid last line"
    
    -- SplIntListDeclare2   Int[] x
    eval ((SplIntListDeclare var), rest, e, level)
        | checkSameVariable var e = error "Duplicate variable"
        | length rest > 0 = eval (head rest, tail rest, (var, "[]", TyIntList, level):e, level)
        | otherwise = error "invalid last line"
    
    -- SplIntMatrixDeclare Int[][] x
    eval ( (SplIntMatrixDeclare var), rest, e, level)
        | checkSameVariable var e = error "Duplicate variable"
        | length rest > 0 = eval (head rest, tail rest, (var, "[]", TyIntMatrix, level):e, level)
        | otherwise = error "invalid last line"
    
    --SplIntMatrix  Int[][] x= streams
    eval ( (SplIntMatrix var ), rest, e, level)  
        | checkSameVariable var e = error "Duplicate variable"
        | length rest > 0 = eval (head rest, tail rest, (var, b, TyIntMatrix, level):e, level)
        | otherwise = error "invalid last line"
        where (a,b,c,d) = getVarTuple "!!!" e
    
    -- push
    eval ( (SplIntListPush var expr), rest, e, level)
        | length rest > 0 && isSplInt reduced_expr && c == TyIntList = eval (head rest, tail rest, currentEnvir ++ [(a,new_int_list1,c,d)], level)
        | length rest > 0 && isSplIntList reduced_expr && c == TyIntMatrix = eval (head rest, tail rest, currentEnvir ++ [(a, new_int_list2 ,c,d)], level ) 
        | length rest == 0 = error "invalid last line"
        where reduced_expr = evalExpr expr e
              currentEnvir = deleteEnvir var e
              (a,b,c,d) = getVarTuple var e
              SplInt x = reduced_expr
              int_list = read b :: [Int]
              new_int_list1 = show (int_list++[x])
              SplIntList y = reduced_expr
              int_list' = read b :: [[Int]]
              new_y = read y :: [Int]
              new_int_list2 = show (int_list'++[new_y])
    
     
    -- pop   x.pop()  IntList   pop   x.pop()  IntMatrix
    eval ( (SplIntListPop var), rest, e, level)
        | length rest > 0 && c == TyIntList && length real_b > 0 =  eval (head rest, tail rest, currentEnvir ++ [(a,new_b,c,d)], level) 
        | length rest > 0 && c == TyIntMatrix && length real_b' > 0 =  eval (head rest, tail rest, currentEnvir ++ [(a,new_b',c,d)], level ) 
        | length real_b < 1 || length real_b'<1 = error "cannot pop because the list is empty"
        | c /= TyIntMatrix = error "The type is not a Intmatrix"
        | c /= TyIntList = error "The type is not a IntList"
        | length rest == 0 = error "invalid last line"
        where currentEnvir = deleteEnvir var e
              (a,b,c,d) = getVarTuple var e
              real_b = read b :: [Int]
              real_b' = read b :: [[Int]]
              new_b = show (init real_b)
              new_b' = show (init real_b')
    
    -- SplAssignment      Exp = Exp
    -- SplAssignment x = 1
    eval ((SplAssignment (name) (SplInt n)), rest,e, level) 
        | get3rd (getVarTuple name e) == TyInt =  eval (head rest, tail rest, newEnv:(deleteEnvir name e), level)
        | rest == [] = error "invalid last line"
        | get3rd (getVarTuple name e) /= TyInt = error "Assignment Value Type Error, Expect Int"
        where newEnv = (changeValue (show n) (getVarTuple name e))
    
    -- SplAssignment x = Bool
    eval ((SplAssignment (name) (SplFalse)), rest,e, level) 
        | get3rd (getVarTuple name e) == TyBool =  eval (head rest, tail rest, newEnv:(deleteEnvir name e), level)
        | rest == [] = error "invalid last line"
        | get3rd (getVarTuple name e) /= TyBool = error "Assignment Value Type Error, Expect Bool"
        where newEnv = (changeValue ("False") (getVarTuple name e))
    
    -- SplAssignment x = Bool
    eval ((SplAssignment (name) (SplTrue)), rest,e, level) 
        | get3rd (getVarTuple name e) == TyBool =  eval (head rest, tail rest, newEnv:(deleteEnvir name e), level)
        | rest == [] = error "invalid last line"
        | get3rd (getVarTuple name e) /= TyBool = error "Assignment Value Type Error, Expect Bool"
        where newEnv = (changeValue ("True") (getVarTuple name e))
    
    --SplAssignment x = Exp
    eval ((SplAssignment (name) exp ), rest,e, level)
        | get3rd (getVarTuple name e) == TyIntList =  eval (head rest, tail rest, newListEnv:(deleteEnvir name e), level)
        | get3rd (getVarTuple name e) == TyInt =  eval (head rest, tail rest, newIntEnv:(deleteEnvir name e), level)
        | rest == [] = error "invalid last line"
        where newIntEnv = (changeValue newValue (getVarTuple name e))
              newListEnv = (changeValue newValue (getVarTuple name e))
              newValue = getString (evalExpr exp e)
    
    
    -- return (x)   return matrix
    eval ( (SplReturn var ), rest , e, level)
        | rest == [] && c == TyIntMatrix = ( (SplReturn var ), rest , e, level)
    -- | rest /= [] = error "invalid last line"
        | c /= TyIntMatrix = error "return type should be SplIntMatrix"
        where (a,b,c,d) = getVarTuple var e
    
    
    
    -- SplIfThen   if (exp ){ ... }
    eval ((SplIfThen expr sen ),rest,e, level)
        | rest /= [] && (myBool == SplTrue) = eval (head new_rest, tail new_rest,e, level)
        | rest /= [] && (myBool == SplFalse) = eval (head rest, tail rest, e, level)  
        | rest == [] = error "invalid last line"
        where sen_list = formatSentence sen
              new_rest = sen_list ++ rest
              myBool = evalExpr expr e
              
    
    eval ( (SplIfThenElse expr sen1 sen2), rest, e , level)
        | rest /= [] && (myBool == SplTrue) = eval ( head new_rest1, tail new_rest2, e, level)
        | rest /= [] && (myBool == SplFalse) = eval ( head new_rest2, tail new_rest2, e, level)
        | rest == [] = error "invalid last line"
        
        where sen1_list = formatSentence sen1
              sen2_list = formatSentence sen2
              new_rest1 = sen1_list ++ rest
              new_rest2 = sen2_list ++ rest
              myBool = evalExpr expr e
    
    
    --SplEnd 
    eval ( SplEnd, rest, e, level) 
        | rest /= [] = eval (head rest, tail rest, new_envir, level-1)
        | rest == [] = error "invalid last line"
        where new_envir = deleteLocalVariable level e
    
    --comments
    eval ( SplComments, rest, e, level)
        | rest /= [] = eval (head rest, tail rest, e, level)
        | rest == [] = error "invalid last line"    
    
    --while
    eval ( (SplWhile expr sen), rest, e, level)
        | rest /= [] && (myBool == SplTrue) = eval ( head new_rest, tail new_rest, e, level+1)
        | rest /= [] && (myBool == SplFalse) = eval (head rest, tail rest, e, level)
        | rest == [] = error "invalid last line"
        where myBool = evalExpr expr e
              sen_list = formatSentence sen
              new_rest = sen_list ++ [SplEnd] ++ [(SplWhile expr sen)] ++ rest
              
    eval x = x        
    
    --Less Than Exp reduction
    evalExpr :: Expr -> Environment -> Expr
    evalExpr (SplLessThan (SplInt n1) (SplInt n2)) e = getBool (n1 < n2) 
    evalExpr (SplLessThan e1 e2) e =  evalExpr (SplLessThan (evalExpr e1 e) (evalExpr e2 e)) e 
    
    --Add Exp reduction
    evalExpr (SplAdd (SplInt n1) (SplInt n2)) e = SplInt (n1+n2)
    evalExpr (SplAdd e1 e2) e = evalExpr (SplAdd (evalExpr e1 e) (evalExpr e2 e)) e
    
    --Substract Exp reduction
    evalExpr (SplSubtract (SplInt n1) (SplInt n2)) e = SplInt (n1-n2) 
    evalExpr (SplSubtract e1 e2) e = evalExpr (SplSubtract (evalExpr e1 e) (evalExpr e2 e)) e
    
    --Divide Exp reduction
    evalExpr (SplDivide (SplInt n1) (SplInt n2)) e = SplInt (n1 `div` n2) 
    evalExpr (SplDivide e1 e2) e = evalExpr (SplDivide (evalExpr e1 e) (evalExpr e2 e)) e
    
    --Multi Exp reduction
    evalExpr (SplMulti (SplInt n1) (SplInt n2)) e = SplInt (n1 * n2) 
    evalExpr (SplMulti e1 e2) e = evalExpr (SplMulti (evalExpr e1 e) (evalExpr e2 e)) e
    
    evalExpr (SplVar name) e 
        | its_type == TyInt = (SplInt newValue)
        | its_type == TyIntList = (SplIntList (show newValue1))
        where newValue = (read (get2nd (getVarTuple name e))::Int)
              newValue1 = (read (get2nd (getVarTuple name e))::[Int])
              its_type = get3rd (getVarTuple name e)
              
    
    
    evalExpr (SplIntListgetElement name exp) e 
        | (get3rd (getVarTuple name e)) == TyIntList = (SplInt newIntValue)
        | (get3rd (getVarTuple name e)) == TyIntMatrix = (SplIntList newStringValue)
        where newIntValue = (getElementinList (n) (read (get2nd (getVarTuple name e))::[Int] ))
              newStringValue = show (getElementinMatrix (n) (read (get2nd (getVarTuple name e))::[[Int]] ))
              (SplInt n) = (evalExpr (exp) e)
    
    evalExpr (SplIntListLength name) e 
        | get3rd  (getVarTuple name e) == TyIntMatrix = (SplInt listLength1)
        | get3rd  (getVarTuple name e) == TyIntList = (SplInt listLength)
        where listLength = length (read (get2nd (getVarTuple name e))::[Int] )
              listLength1 = length (read (get2nd (getVarTuple name e))::[[Int]] )
       
    
    evalExpr (SplIsEqual(SplInt n1) (SplInt n2)) e = getBool (n1 == n2) 
    evalExpr (SplIsEqual e1 e2) e =  evalExpr (SplIsEqual (evalExpr e1 e) (evalExpr e2 e)) e 
    
    evalExpr (SplInt n) e = (SplInt n)
    
    evalExpr (SplIntList v) e = (SplIntList v)
    
    evalExpr (SplFalse) e = (SplFalse) 
    
    evalExpr (SplTrue) e = (SplTrue) 
    
    evalExpr SplStream e = SplStream
    
    
    getVarTuple :: String -> Environment -> (String,String,SplType,Int)
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
    
    get1st (a,_,_,_) = a
    
    get2nd (_,b,_,_) = b
    
    get3rd (_,_,c,_) = c
    
    get4th (_,_,_,d) = d
    
    
    
    changeValue :: String -> (String,String,SplType,Int) -> (String,String,SplType,Int)
    changeValue newValue varTuple = (get1st varTuple,newValue,get3rd varTuple,get4th varTuple)
    
    getElementinList :: Int -> [Int] -> Int
    getElementinList 0 (x:xs) = x
    getElementinList n (x:xs) = getElementinList (n-1) xs
    getElementinList _ [] = error "out of list limit" 
    
    getElementinMatrix :: Int -> [[Int]] -> [Int]
    getElementinMatrix 0 (x:xs) = x
    getElementinMatrix n (x:xs) = getElementinMatrix (n-1) xs
    getElementinMatrix _ [] = error "out of list limit" 
    
    getString :: Expr -> String
    getString (SplInt n) = show n
    getString (SplIntList x) = x
    
    isSplInt :: Expr -> Bool
    isSplInt (SplInt x) = True
    isSplInt _ = False
    
    isSplIntList :: Expr -> Bool
    isSplIntList (SplIntList x) = True 
    isSplIntList _ = False
    
    deleteLocalVariable :: Int ->  Environment -> Environment
    deleteLocalVariable level (x:xs)
        | get4th x == level = [] ++ deleteLocalVariable level xs
        | get4th x /= level = [x] ++ deleteLocalVariable level xs
    deleteLocalVariable _ [] = []