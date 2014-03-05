module Main where
import System.Environment (getArgs)
import Data.List (group)

rle :: [String] -> [String]
rle lat = [show len, ele]
	where len = length lat
              ele = head lat

processLine :: String -> String
processLine = unwords . concat . map rle . group . words

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
