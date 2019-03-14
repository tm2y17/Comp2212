import SplTokens
import System.Environment
import Control.Exception
import System.IO

--按顺序parse每一行指令,把parse的exp放在一个list里面
main = do
    --args <- getArgs
    content <- readFile "test.txt"
    let linesOfFiles = (alexScanTokensLines (lines content))
    --let linesOfFiles = alexScanTokensLines (lines content)
    print linesOfFiles

--按顺序把每一行的char转换成tokens
alexScanTokensLines :: [[Char]] -> [[SplToken]]
alexScanTokensLines [] = []
alexScanTokensLines (xs:xss) =  alexScanTokens xs : alexScanTokensLines xss
