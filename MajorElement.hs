module Main where
import System.Environment (getArgs)
import qualified Data.Map as Map

increment :: Map.Map String Int -> String -> (Map.Map String Int, Int)
increment map a = case Map.insertLookupWithKey (\_ a b -> a + b) a 1 map of
	(Just val, m) -> (m, val + 1)
	(Nothing, m) -> (m, 0)

process :: Int -> [String] -> Map.Map String Int -> Maybe String
process req [] _ = Nothing
process req (x:xs) map = if frequency > req then Just x else process req xs map'
	where (map', frequency) = increment map x

showProcess :: Maybe String -> String
showProcess Nothing = "None"
showProcess (Just x) = x

processLine :: String -> String
processLine l = showProcess $ process req entries Map.empty
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
