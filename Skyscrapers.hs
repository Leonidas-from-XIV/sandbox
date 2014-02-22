module Main where
import System.Environment (getArgs)
import Data.Function (on)
import Data.Char (isDigit)
import Data.List (maximumBy, sortBy, groupBy, insertBy, delete)
--import Debug.Trace (trace)

trace _ x = x

data Building = Building {l::Int,  h::Int, r::Int} deriving Show

parseTuple :: String -> Building
parseTuple s = case wordsWhen (== ',') $ takeWhile (/= ')') $ dropWhile (not . isDigit) s of
	[sl,sh,sr] -> Building (read sl::Int) (read sh::Int) (read sr::Int)

processLine :: String -> String
processLine line = unwords $ map show $ reformat $ mergeHeights [0] [] bs es
	where buildings = map parseTuple $ wordsWhen (== ';') line
              bs = begins buildings
              es = ends buildings

begins :: [Building] -> [Building]
--begins xs = map (maximumBy (compare `on` h)) $ groupBy ((==) `on` l) $ sortBy (compare `on` l) xs
begins xs = sortBy (compare `on` l) xs
ends :: [Building] -> [Building]
--ends xs = map (maximumBy (compare `on` h)) $ groupBy ((==) `on` r) $ sortBy (compare `on` r) xs
ends xs = sortBy (compare `on` r) xs

insertOrdered x xs = insertBy (compare `on` negate) x xs

mergeHeights :: [Int] -> [(Int,Int)] -> [Building] -> [Building] -> [(Int,Int)]
mergeHeights [] res _ _ = res
mergeHeights _ res [] [] = res
mergeHeights _ res (_:_) [] = res
mergeHeights (c:cs) res [] (e:es)
	| h e < c = mergeHeights (c:(delete (h e) cs)) res [] es
	| h e == c = mergeHeights cs ((r e, head cs):res) [] es
	| otherwise = error "Problem with ends"
mergeHeights (c:cs) res (b:bs) (e:es)
	| l b <= r e && h b >= c = trace "branch 1" $ traceCS (h b:c:cs) $ mergeHeights (h b:c:cs) (((l b), h b):res) bs (e:es)
	| l b <= r e = trace "branch 2" $ traceCS (c:(insertOrdered (h b) cs)) $ mergeHeights (c:(insertOrdered (h b) cs)) res bs (e:es)
	| r e < l b && h e < c = trace "branch 3" $ traceCS (c:(delete (h e) cs)) $ mergeHeights (c:(delete (h e) cs)) res (b:bs) es
	| r e < l b && h e == c = trace "branch 4" $ traceCS cs $ mergeHeights cs ((r e, head cs):res) (b:bs) es
	| otherwise = error ("Problem with recursion" ++ (show (h e)) ++ " " ++ (show c))

traceCS :: Show a => a -> t -> t
traceCS cs = trace $ show cs

reformat :: [(Int,Int)] -> [Int]
reformat lat = concat [[a,b] | (a,b) <- reverse lat]

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
