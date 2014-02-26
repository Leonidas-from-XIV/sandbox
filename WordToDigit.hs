module Main where
import System.Environment (getArgs)

wordToDigit :: String -> Char
wordToDigit "zero" = '0'
wordToDigit "one" = '1'
wordToDigit "two" = '2'
wordToDigit "three" = '3'
wordToDigit "four" = '4'
wordToDigit "five" = '5'
wordToDigit "six" = '6'
wordToDigit "seven" = '7'
wordToDigit "eight" = '8'
wordToDigit "nine" = '9'
wordToDigit _ = '_'

processLine :: String -> String
processLine l = map wordToDigit $ wordsWhen (== ';') l

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
