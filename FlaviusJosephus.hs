module Main where
import System.Environment (getArgs)
--import Debug.Trace (trace)

trace _ x = x

breakNth _ [] = ([], [])
breakNth n lat = case splitAt (n-1) lat of
	(skipped,r:rs) -> let (skipped', rest) = breakNth n rs in
		(skipped ++ skipped', r:rest)
	(skipped, []) -> (skipped, [])

skipBreak 0 n lat = breakNth n lat
skipBreak skip n lat = let (_:skipped,matched) = skipBreak (skip-1) n (0:lat)
	in (skipped,matched)

process _ _ [] = []
process s n lat = let (skipped, died) = skipBreak s n lat
                      nextSkip = ((length lat `mod` n) + s) `mod` n
	in trace ("nextSkip " ++ show nextSkip) $ died ++ process nextSkip n skipped

flaviusJosephus = process 0

processLine :: String -> String
processLine l = l

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
