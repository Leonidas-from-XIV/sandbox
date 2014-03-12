module Main where
import System.Environment (getArgs)
import Data.List (tails)

data Entry = Literal Char | Star deriving (Show)
type Glob = [Entry]

construct :: String -> Glob
construct "*" = [Star]
construct [x] = [Literal x]
construct (x:y:xs) = if x == '\\' && y == '*' then (Literal '*'):(construct xs) else
	(if x == '*' then Star else Literal x):(construct (y:xs))

matchString :: Glob -> String -> Bool
matchString [] _ = True
matchString [Star] _ = True
matchString [_] [] = False
matchString (Literal g:gs) (x:xs) = x == g && matchString gs xs
matchString (Star:gs) str = any (== True) $ map (matchString gs) $ tails str

processLine :: String -> String
processLine l = l

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
