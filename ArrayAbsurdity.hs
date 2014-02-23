module Main where
import System.Environment (getArgs)
import Data.List (sort)

findDupe :: [Int] -> Int
findDupe [] = 0
findDupe [_] = 0
findDupe (y:x:xs)
	| x == y = x
	| otherwise = findDupe (x:xs)

processLine :: String -> String
processLine l = show $ findDupe $ sort entries
        where entries = read ("[" ++ items ++ "]")::[Int]
              [_,items] = wordsWhen (== ';') l

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
