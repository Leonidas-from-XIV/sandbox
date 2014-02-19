module Main where
import System.Environment (getArgs)
import Data.List (sort, intercalate)

possibilities :: Char -> String
possibilities '0' = "0"
possibilities '1' = "1"
possibilities '2' = "abc"
possibilities '3' = "def"
possibilities '4' = "ghi"
possibilities '5' = "jkl"
possibilities '6' = "mno"
possibilities '7' = "pqrs"
possibilities '8' = "tuv"
possibilities '9' = "wxyz"
possibilities _ = ""

-- for some interesting reason >>= is the same as flatMap
addPossibility :: [String] -> Char -> [String]
addPossibility acc x = acc >>= (\tail -> map (:tail) $ possibilities x)

processLine :: String -> String
processLine l = intercalate "," $ sort $ foldl addPossibility [""] $ reverse l

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
