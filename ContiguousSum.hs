module Main where
import System.Environment (getArgs)

contiguous :: [Int] -> [[Int]]
contiguous [] = []
contiguous [a] = [[a]]
contiguous (x:xs) = tailSeq (x:xs) ++ contiguous xs

tailSeq :: [Int] -> [[Int]]
tailSeq [a] = [[a]]
tailSeq (x:xs) = [x]:(map (x:) $ tailSeq xs)

processLine :: String -> String
processLine l = show $ foldl (\a e -> max a $ sum e) x $ contiguous (x:xs)
	where (x:xs) = map (\x -> (read x::Int)) $ wordsWhen (== ',') $ filter (/= ' ') l

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
	"" -> []
	s' -> w : wordsWhen p s''
		where (w, s'') = break p s'

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
