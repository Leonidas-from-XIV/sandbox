module Main where
import System.Environment (getArgs)

matchString :: String -> String -> Bool
matchString _ [] = True
matchString [] [_] = False
matchString (x:xs) [e]
	| x == e = True
	| x /= e = matchString xs e


processLine :: String -> String
processLine l = l

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
