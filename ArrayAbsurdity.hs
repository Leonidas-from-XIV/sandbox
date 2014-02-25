module Main where
import System.Environment (getArgs)

sumTo :: Int -> Int
sumTo n = (n^2 + n) `quot` 2

processLine :: String -> String
processLine l = show $ sum entries - sumTo (n-2)
        where entries = read ("[" ++ items ++ "]")::[Int]
              n = read ns::Int
              [ns,items] = wordsWhen (== ';') l

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
