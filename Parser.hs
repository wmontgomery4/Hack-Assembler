-- This file acts as the entire assembler, rather than building separate modules.
-- Also, many of the API functions are missing.  This is because TECS implicitly
-- assumes that we are using an imperative language in it's API specification.
-- However, the user can still use this file to assemble an .asm file by running
-- this file with an .asm file as the only command line argument.

-- Right now there's not much error handling, but I'll add it in eventually once
-- I get it up and running.


import Data.Char
import Data.List
import System.Environment
import System.IO

data CommandType = ACommand | CCommand | LCommand deriving (Show)
type Command = String	

main = do
     [asmFile] <- getArgs
     asmHandle <- openFile asmFile ReadMode
     asmContents <- hGetContents asmHandle
     let processed = process asmContents
     	 tupleList = map (\cmd -> (cmd,commandType cmd)) processed
     mapM_ (putStrLn . show) tupleList

-- Converts the file to a list of parsable commands, gets rid of white space too
process :: String -> [Command]
process = filter isCommand . map (filter isNotSpace) . lines
	where isNotSpace = not . isSpace
	      isCommand str = (str /= "") && not (isPrefixOf "//" str)


-- Returns the command type of a command, assumes well-formed expressions right now
commandType :: Command -> CommandType
commandType cmd 
	    | head cmd == '(' = LCommand
	    | head cmd == '@' = ACommand
	    | otherwise       = CCommand 

