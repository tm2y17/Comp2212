module SplEval where
import SplGrammar
    
type Environment = [(String,String)]
type State = (Sentence,[Sentence],Environment)


formatSentence :: Sentence -> [Sentence]
formatSentence (SplConnecting x1 x2) = formatSentence x1 ++ formatSentence x2
formatSentence x = [x]

parseToState :: Sentence -> State
parseToState s = (head (formatSentence s), tail (formatSentence s),[])

{-
getMatrix :: Sentence -> Environment -> String


evalLoop :: State -> [[Int]] -> String
evalLoop (current, rest, e) int_list 
    | isReturn current = getMatrix current e
    | !(isReturn current) = evalLoop (eval (current, rest, e))
-}

eval :: State -> State
-- SplIntDeclare  Int i = 3
eval ( (SplIntDeclare var (SplInt x) ),rest, e) 
    | checkSameVariable var e = error "same name variables"
    | length rest > 0 = eval (head rest, tail rest, (var,(show x)):e )
    | otherwise = error "invalid last line"
eval ( (SplIntDeclare var _ ),rest, e) = error "Invalid variable, Int expected"

-- SplBoolDeclare  Bool x = false
eval ( (SplBoolDeclare var SplTrue ),rest, e) 
    | checkSameVariable var e = error "same name variables"
    | length rest > 0 = eval (head rest, tail rest, (var, "True"):e )
    | otherwise = error "invalid last line"
eval ( (SplBoolDeclare var SplFalse ),rest, e) 
    | checkSameVariable var e = error "same name variables"
    | length rest > 0 = eval (head rest, tail rest, (var, "False"):e )
    | otherwise = error "invalid last line"
eval ( (SplBoolDeclare var _),rest, e) = error "Invalid variable, Bool expected"

-- SplIntListDeclare1   Int[] x = [1,2,3]
eval ( (SplIntListAssignment var (SplIntList x)) , rest, e)
    | checkSameVariable var e = error "same name variables"
    | length rest > 0 = eval (head rest, tail rest, (var, (show x) ):e )
    | otherwise = error "invalid last line"

-- SplIntListDeclare2   Int[] x
eval ((SplIntListDeclare var), rest, e )
    | checkSameVariable var e = error "same name variables"
    | length rest > 0 = eval (head rest, tail rest, (var, "[]"):e )
    | otherwise = error "invalid last line"

-- SplIntMatrixDeclare Int[][] x
eval ( (SplIntMatrixDeclare var), rest, e)
    | checkSameVariable var e = error "same name variables"
    | length rest > 0 = eval (head rest, tail rest, (var, "[]"):e )
    | otherwise = error "invalid last line"





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