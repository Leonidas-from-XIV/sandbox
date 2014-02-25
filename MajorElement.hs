module Main where
import System.Environment (getArgs)

increment :: [(String,Int)] -> String -> ([(String,Int)], Int)
increment [] a = ([(a, 1)], 1)
increment ((a,b):xs) a'
	| a' == a = ((a, (b+1)):xs, (b+1))
	| otherwise = ((a,b):map, val)
            where (map, val) = increment xs a'

process :: Int -> [String] -> [(String,Int)] -> Maybe String
process req [] _ = Nothing
process req (x:xs) map = if frequency > req then Just x else process req xs map'
	where (map', frequency) = increment map x

showProcess :: Maybe String -> String
showProcess Nothing = "None"
showProcess (Just x) = x

processLine :: String -> String
processLine l = showProcess $ process req entries []
	where req = length entries `quot` 2
	      entries = wordsWhen (== ',') l

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
