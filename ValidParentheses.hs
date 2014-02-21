module Main where
import System.Environment (getArgs)

data Validation = Valid [Char] | Invalid deriving Show

-- String, since that saves us formatting afterwars
validate :: Validation -> String
validate (Valid []) = "True"
validate _ = "False"

processChar acc c = case (c, acc) of
	('(', Valid xs) -> Valid (c:xs)
	('{', Valid xs) -> Valid (c:xs)
	('[', Valid xs) -> Valid (c:xs)
	(')', Valid []) -> Invalid
	('}', Valid []) -> Invalid
	(']', Valid []) -> Invalid
	(')', Valid (x:xs)) -> if x == '(' then Valid xs else Invalid
	('}', Valid (x:xs)) -> if x == '{' then Valid xs else Invalid
	(']', Valid (x:xs)) -> if x == '[' then Valid xs else Invalid
	otherwise -> acc

processLine :: String -> String
processLine l = validate $ foldl processChar (Valid []) l

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
