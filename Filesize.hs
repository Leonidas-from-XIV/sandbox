module Main where
	import System.Environment (getArgs)
	import System.Posix (getFileStatus, fileSize)

	main = do
		[inpFile] <- getArgs
		stat <- getFileStatus inpFile
		putStrLn $ show $ fileSize stat
