module Main where
import System.Environment (getArgs)
import Data.List (intercalate)

pairSums :: [Int] -> Int -> [(Int, Int)]
pairSums nums desired = [(x,y) | x <- nums, y <- nums, x<y, x+y==desired]

showTuple :: (Int,Int) -> String
showTuple (a,b) = (show a) ++ "," ++ (show b)

showSums :: [(Int, Int)] -> String
showSums [] = "NULL"
showSums x = intercalate ";" $ map showTuple x

processLine :: String -> String
processLine l = case wordsWhen (== ';') l of
	[nums,wanted] -> showSums $ pairSums (read ("[" ++ nums ++ "]")::[Int]) (read wanted::Int)
	otherwise -> "Invalid input"

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
	"" -> []
	s' -> w : wordsWhen p s''
		where (w, s'') = break p s'

main :: IO()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
