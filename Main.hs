import SplTokens
import SplGrammar
import System.Environment
import Control.Exception
import System.IO
import Data.List
import SplEval

main :: IO ()
main = catch main' noParse

main' = do (fileName : _ ) <- getArgs 
           sourceText <- readFile fileName
           putStrLn ("Parsing : " ++ sourceText)
           let parsedProg = parseCalc (alexScanTokens sourceText)
           putStrLn ("Parsed as " ++ (show parsedProg) ++ "\n")

           content <-  getContents
           let int_List = transfer (lines content)
           
           let result = evalLoop (parseToState parsedProg (transform int_List) )

           putStrLn ("Matrix: "++result)
           --[[Int]]
           putStrLn ("    ")
           putStrLn ("standard output: ")
           sequence_ $ (matrixToStringList (transform (read result :: [[Int]]) ) ) >>= (\x -> [putStrLn x])

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()

transfer :: [String] -> [[Int]]
transfer (x:xs) = [(map read $ words x :: [Int])] ++ transfer xs
transfer [] = []

toSpaceSeparatedString :: [String] -> String
toSpaceSeparatedString = intercalate " "

matrixToStringList :: [[Int]] -> [String]
matrixToStringList (x:xs) = [(toSpaceSeparatedString (map show x))]++matrixToStringList xs
matrixToStringList [] = []

transform:: [[a]]->[[a]]
transform ([]:_) = []
transform x = (map head x) : transform (map tail x)