module Main where
import System.Environment (getArgs)
import Data.Function (on)
import Data.Char (isDigit)
import Data.List (sortBy, insertBy, delete, maximumBy)
--import Debug.Trace (trace)

trace _ x = x

data Building = Building {l::Int, r::Int, h::Int} deriving (Show, Eq, Ord)

parseTuple :: String -> Building
parseTuple s = case wordsWhen (== ',') $ takeWhile (/= ')') $ dropWhile (not . isDigit) s of
	[sl,sh,sr] -> Building (read sl::Int) (read sr::Int) (read sh::Int)

processLine :: String -> String
processLine line = unwords $ map show $ reformat $ mergeHeights [0] [] bs es
	where buildings = map parseTuple $ wordsWhen (== ';') line
              bs = begins buildings
              es = ends buildings

begins :: [Building] -> [Building]
begins xs = sortBy (compare `on` l) xs
ends :: [Building] -> [Building]
ends xs = sortBy (compare `on` r) xs

insertOrdered = insertBy (compare `on` negate)
maximumSeq = maximumBy (compare `on` r)

mergeHeights :: [Int] -> [(Int,Int)] -> [Building] -> [Building] -> [(Int,Int)]
mergeHeights [] res _ _ = res
mergeHeights _ res [] [] = res
mergeHeights _ res (_:_) [] = res
mergeHeights (c:cs) res [] (e:es)
	| h e < c = mergeHeights (delete (h e) (c:cs)) res [] es
	| h e == c = mergeHeights (delete (h e) (c:cs)) ((r e, head cs):res) [] es
	| otherwise = error "Problem with ends"
mergeHeights (c:cs) res (b:bs) (e:es)
	| l b <= r e && h b >= c = trace "branch 1" $ mergeHeights (insertOrdered (h b) (c:cs)) (((l b), h b):res) bs (e:es)
	| l b <= r e = trace "branch 2" $ mergeHeights (insertOrdered (h b) (c:cs)) res bs (e:es)
	| r e < l b && h e < c = trace "branch 3" $ mergeHeights (delete (h e) (c:cs)) res (b:bs) es
	| r e < l b && h e == c = trace "branch 4" $ mergeHeights (delete (h e) (c:cs)) ((r e, head cs):res) (b:bs) es
	| otherwise = error ("Problem with recursion" ++ (show (h e)) ++ " " ++ (show c))

traceCS :: Show a => a -> t -> t
traceCS cs = trace $ show cs

-- down and up again
removeIdentical :: [(Int, Int)] -> [(Int, Int)]
removeIdentical [] = []
removeIdentical [a] = [a]
removeIdentical ((a,b):(a',b'):xs)
	| b == b' = removeIdentical ((a,b):xs)
	| otherwise = (a,b):(removeIdentical ((a',b'):xs))

removeDouble :: [(Int, Int)] -> [(Int, Int)]
removeDouble [] = []
removeDouble [a] = [a]
removeDouble ((a,b):(a',b'):xs)
	| a == a' = removeDouble ((a',b'):xs)
	| otherwise = (a,b):(removeDouble ((a',b'):xs))

reformat :: [(Int,Int)] -> [Int]
reformat lat = concat [[a,b] | (a,b) <- removeDouble $ removeIdentical $ reverse lat]

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
