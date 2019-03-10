import Data.List

main :: IO ()
main = do 
    content <-  getContents
    putStrLn ("content: "++ "\n"++content)
    putStrLn ("    ")

    putStrLn ("matrix: ")
    let int_List = transfer (lines content)
    print int_List
    --int_List :: [[Int]] 
    putStrLn ("    ")

    putStrLn ("standard output: ")
    sequence_ $ (matrixToStringList int_List) >>= (\x -> [putStrLn x])

transfer :: [String] -> [[Int]]
transfer (x:xs) = [(map read $ words x :: [Int])] ++ transfer xs
transfer [] = []

toSpaceSeparatedString :: [String] -> String
toSpaceSeparatedString = intercalate " "

matrixToStringList :: [[Int]] -> [String]
matrixToStringList (x:xs) = [(toSpaceSeparatedString (map show x))]++matrixToStringList xs
matrixToStringList [] = []