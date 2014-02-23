module Main where
import System.Environment (getArgs)
import Data.Function (on)
import Data.Char (isDigit)
import Data.List (sortBy, insertBy, delete)

data Building = Building {l::Int, r::Int, h::Int} deriving Show

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

insertOrdered :: Int -> [Int] -> [Int]
insertOrdered = insertBy (compare `on` negate)

mergeHeights :: [Int] -> [(Int,Int)] -> [Building] -> [Building] -> [(Int,Int)]
mergeHeights [] res _ _ = res
mergeHeights _ res [] [] = res
mergeHeights _ res (_:_) [] = res
mergeHeights (c:cs) res [] (e:es)
	| h e < c = mergeHeights (delete (h e) (c:cs)) res [] es
	| h e == c = mergeHeights (delete (h e) (c:cs)) ((r e, head cs):res) [] es
	| otherwise = error "Assertion at merging ends failed"
mergeHeights (c:cs) res (b:bs) (e:es)
	| l b <= r e && h b >= c = mergeHeights (insertOrdered (h b) (c:cs)) (((l b), h b):res) bs (e:es)
	| l b <= r e = mergeHeights (insertOrdered (h b) (c:cs)) res bs (e:es)
	| r e < l b && h e < c = mergeHeights (delete (h e) (c:cs)) res (b:bs) es
	| r e < l b && h e == c = mergeHeights (delete (h e) (c:cs)) ((r e, head cs):res) (b:bs) es
	| otherwise = error ("Assertion at merging bs/es failed: " ++ (show (h e)) ++ " " ++ (show c))

removePeaksAndDoubles :: [(Int, Int)] -> [(Int, Int)]
removePeaksAndDoubles [] = []
removePeaksAndDoubles [a] = [a]
removePeaksAndDoubles ((a,b):(a',b'):xs)
	-- goes to some height and then continues at the same height, pick former
	| b == b' = removePeaksAndDoubles ((a,b):xs)
	-- the something ends and starts at the same place, pick latter
	| a == a' = removePeaksAndDoubles ((a',b'):xs)
	| otherwise = (a,b):(removePeaksAndDoubles $ (a', b'):xs)

reformat :: [(Int,Int)] -> [Int]
reformat lat = concat [[a,b] | (a,b) <- removePeaksAndDoubles $ reverse lat]

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
