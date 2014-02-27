module Main where
import System.Environment (getArgs)

breakNth _ [] = ([], [])
breakNth n lat = case splitAt (n-2) lat of
	(skipped,r:rs) -> let (skipped', rest) = breakNth n rs in
		(skipped ++ skipped', x:r:rest)
	(skipped, []) -> (skipped, x:[])

processLine :: String -> String
processLine l = l

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
