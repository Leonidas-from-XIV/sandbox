module Main where
import System.Environment (getArgs)

endsWith :: String -> String -> Bool
endsWith end s = rest == end
                 where rest = drop n s
                       n = length s - length end

showBool :: Bool -> String
showBool True = "1"
showBool False = "0"

processLine :: String -> String
processLine l = case wordsWhen (== ',') l of
	[s,end] -> showBool $ endsWith end s
	otherwise -> "Invalid input"

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
	"" -> []
	s' -> w : wordsWhen p s''
		where (w, s'') = break p s'

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
