import SplTokens
import SplGrammar
import System.Environment
import Control.Exception
import System.IO
import Data.List

main :: IO ()
main = catch main' noParse

main' = do (fileName : _ ) <- getArgs 
           sourceText <- readFile fileName
           putStrLn ("Parsing : " ++ sourceText)
           let parsedProg = parseCalc (alexScanTokens sourceText)
           putStrLn ("Parsed as " ++ (show parsedProg) ++ "\n")

           content <-  getContents
           let int_List = transfer (lines content)
           print int_List

           --putStrLn ("    ")
           --putStrLn ("standard output: ")
           --sequence_ $ (matrixToStringList int_List) >>= (\x -> [putStrLn x])

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