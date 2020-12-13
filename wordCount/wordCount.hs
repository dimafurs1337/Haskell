import Data.Char (toLower)
import Data.List (sort, group)
import Control.Arrow ((&&&))
import System.Environment (getArgs)

wordCount :: String -> [(String, Int)]
wordCount = map (head &&& length) . group . sort . words . map toLower

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    fileContent <- readFile inputFile
    print (wordCount fileContent)