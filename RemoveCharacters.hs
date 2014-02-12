module Main where
import System.Environment (getArgs)

removeCharacters :: [Char] -> String -> String
removeCharacters chars s = foldl (\acc e -> filter (/= e) acc) s chars

processLine :: String -> String
processLine l = case wordsWhen (== ',') l of
	[s,(_:rem)] -> removeCharacters rem s
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

