module Main where
import System.Environment (getArgs)

processLine :: String -> String
processLine l = case wordsWhen (== '|') l of
	[s,indices] -> map (s!!) [(read n::Int) - 1 | n <- (words indices)]
	otherwise -> "Incorrect input"

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
	"" -> []
	s' -> w : wordsWhen p s''
		where (w, s'') = break p s'

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
