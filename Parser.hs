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

data Command = ACommand String | LCommand String | CCommand String String String deriving (Show)

main = do
     [asmFile] <- getArgs
     asmHandle <- openFile asmFile ReadMode
     asmContents <- hGetContents asmHandle
     let processed = process asmContents
     	 parsed = map parse processed
     mapM_ (putStrLn . show) parsed


-- Helper function that removes the comments in a line.  We assume that the only use of a
-- forward slash '/' is for comments, because this makes the code much simpler
removeComments :: String -> String
removeComments str = let (precomm,postcomm) = break (=='/') str
	       	     in precomm

-- Converts the file to a list of parsable commands, gets rid of white space too
-- THIS NEEDS TO FIXED TO REMOVE COMMENTS AS WELL
process :: String -> [String]
process contents = let stripped = map (filter (not . isSpace) ) . lines $ contents
		       nocomments = map removeComments stripped
		   in  filter (not . null) nocomments



-- The parse function takes a stripped string (containing only a command)
-- and converts it into one of the Command types, along with its 
-- associated symbol or dest/comp/jump mnemonics

parse :: String -> Command
parse cmd
      | null cmd = error "Parse should not be called on empty strings"
      | head cmd == '(' = LCommand (tail . init $ cmd)
      | head cmd == '@' = ACommand (tail cmd)
      | otherwise       = let (presemi,postsemi) = break (==';') cmd
      			      jump = if null postsemi
			      	     	then ""
					else (tail postsemi)
			      (preeq,posteq) = break (=='=') presemi
			      (dest,comp) = if null posteq
			      		       then ("", preeq)
					       else (preeq, tail posteq)
			  in
			      CCommand dest comp jump


