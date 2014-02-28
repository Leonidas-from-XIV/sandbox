module Main where
import System.Environment (getArgs)
--import Debug.Trace (trace)

trace _ x = x

breakNth :: Int -> [Int] -> ([Int], [Int])
breakNth _ [] = ([], [])
breakNth n lat = case splitAt (n-1) lat of
	(skipped,r:rs) -> let (skipped', rest) = breakNth n rs in
		(skipped ++ skipped', r:rest)
	(skipped, []) -> (skipped, [])

skipBreak :: Int -> Int -> [Int] -> ([Int], [Int])
skipBreak 0 n lat = breakNth n lat
skipBreak skip n lat = let (_:skipped,matched) = skipBreak (skip-1) n (0:lat)
	in (skipped,matched)

process :: Int -> Int -> [Int] -> [Int]
process _ _ [] = []
process s n lat = let (skipped, died) = skipBreak s n lat
                      nextSkip = ((length lat `mod` n) + s) `mod` n
	in trace ("nextSkip " ++ show nextSkip) $ died ++ process nextSkip n skipped

flaviusJosephus :: Int -> [Int] -> [Int]
flaviusJosephus = process 0

processLine :: String -> String
--processLine l = l
processLine l = case wordsWhen (== ',') l of
	[n,m] -> unwords . map show $ flaviusJosephus (read m::Int) [0..((read n::Int)-1)]
	otherwise -> "Invalid input"

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
