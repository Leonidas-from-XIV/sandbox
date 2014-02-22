module Main where
import System.Environment (getArgs)
import Data.Function (on)
import Data.List (maximumBy, sortBy, groupBy)
--import Debug.Trace (trace)

trace _ x = x

data Building = Building {l::Int,  h::Int, r::Int} deriving Show

parseTuple :: String -> Building
parseTuple s = case wordsWhen (== ',') $ takeWhile (/= ')') $ drop 1 s of
	[l,h,r] -> Building (read l::Int) (read h::Int) (read r::Int)

processLine :: String -> String
processLine l = unwords $ map show $ reformat $ mergeHeights [0] [] bs es
	where buildings = map parseTuple $ wordsWhen (== ';') l
              bs = begins buildings
              es = ends buildings

begins xs = map (maximumBy (compare `on` h)) $ groupBy ((==) `on` l) $ sortBy (compare `on` l) xs
ends xs = map (maximumBy (compare `on` h)) $ groupBy ((==) `on` r) $ sortBy (compare `on` r) xs

mergeHeights :: [Int] -> [(Int,Int)] -> [Building] -> [Building] -> [(Int,Int)]
mergeHeights _ [] [] [] = []
-- last building
mergeHeights _ res [] [last] = (r last, 0):res
mergeHeights (c:cs) res [] (e:es)
	| h e < c = mergeHeights (c:cs) res [] es
	| h e == c = mergeHeights cs ((r e, head cs):res) [] es
mergeHeights (c:cs) res (b:bs) (e:es)
	| l b <= r e && h b >= c = trace "branch 1" $ traceCS (c:cs) $ mergeHeights (h b:c:cs) (((l b), h b):res) bs (e:es)
	| l b <= r e = trace "branch 2" $ traceCS (c:cs) $ mergeHeights (c:cs) res bs (e:es)
	| r e < l b && h e < c = trace "branch 3" $ traceCS (c:cs) $ mergeHeights (c:cs) res (b:bs) es
	| r e < l b && h e == c = trace "branch 4" $ traceCS (c:cs) $ mergeHeights cs ((r e, head cs):res) (b:bs) es

traceCS cs = trace $ show cs

reformat :: [(Int,Int)] -> [Int]
reformat r = concat [[a,b] | (a,b) <- reverse r]

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
	"" -> []
	s' -> w : wordsWhen p s''
		where (w, s'') = break p s'

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
