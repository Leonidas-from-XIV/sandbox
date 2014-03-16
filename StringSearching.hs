module Main where
import System.Environment (getArgs)
import Data.List (tails)

data Entry = Literal Char | Star deriving (Show)
type Glob = [Entry]

construct :: String -> Glob
construct [] = []
construct ['*'] = [Star]
construct [x] = [Literal x]
construct (x:y:xs) = if x == '\\' && y == '*' then (Literal '*'):(construct xs) else
	(if x == '*' then Star else Literal x):(construct (y:xs))

matchString :: Glob -> String -> Bool
matchString [] _ = True
matchString [Star] _ = True
matchString [_] [] = False
matchString (Literal _:_) [] = False
matchString (Literal g:gs) (x:xs) = x == g && matchString gs xs
matchString (Star:gs) str = or $ map (matchString gs) $ tails str

processLine :: String -> String
processLine l = case wordsWhen (== ',') l of
	[str,pattern] -> showBool $ matchString (Star:(construct pattern)) str
	_ -> "Invalid input"

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
	"" -> []
	s' -> w : wordsWhen p s''
		where (w, s'') = break p s'

showBool :: Bool -> String
showBool True = "true"
showBool _ = "false"

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
