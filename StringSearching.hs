module Main where
import System.Environment (getArgs)

data Entry = Literal Char | Star deriving (Show)

construct :: String -> [Entry]
construct "*" = [Star]
construct [x] = [Literal x]
construct (x:y:xs) = if x == '\\' && y == '*' then (Literal '*'):(construct xs) else
	(if x == '*' then Star else Literal x):(construct (y:xs))

--matchString :: String -> String -> Bool
--matchString _ [] = True
--matchString [] [_] = False
--matchString (x:xs) [e]
--	| x == e = True
--	| x /= e = matchString xs e


processLine :: String -> String
processLine l = l

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
