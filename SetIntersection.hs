module Main where
import System.Environment (getArgs)
import Data.List (intercalate, elem)

unInt :: [Int] -> String
unInt [] = ""
unInt xs = intercalate "," $ map show xs

splitBy :: Char -> String -> [String]
splitBy item l = wordsWhen (== item) l

toInt :: String -> Int
toInt s = read s::Int

processLine :: String -> String
processLine l = unInt $ case map (\y -> map toInt $ splitBy ',' y) $ splitBy ';' l of
	[a, b] -> intersect a b
	otherwise -> []

intersect :: [Int] -> [Int] -> [Int]
intersect a b = filter inA b
	where inA = flip elem a

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
	"" -> []
	s' -> w : wordsWhen p s''
		where (w, s'') = break p s'

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
