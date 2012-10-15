-- This file acts as the entire assembler, rather than building separate modules.
-- Also, many of the API functions are missing.  This is because TECS implicitly
-- assumes that we are using an imperative language in it's API specification.
-- However, the user can still use this file to assemble an .asm file by running
-- this file with an .asm file as the only command line argument.

-- Right now there's not much error handling, but I'll add it in eventually once
-- I get it up and running.


import Data.Char
import Data.List
import qualified Data.Map as M
import System.Environment
import System.IO

data Command = ACommand String | LCommand String | CCommand String String String deriving (Show)

main = do
     [asmFile] <- getArgs
     let (fileName,period:filetype) = break (=='.') asmFile
     asmContents <- readFile asmFile
     let machineCode = commandsToCode . map parse . process $ asmContents
     	 hackFileName = fileName ++ ".hack"
     writeFile hackFileName machineCode

-- Converts the file to a list of parsable commands, gets rid of white space too

process :: String -> [String]
process contents = let nocomments = map (takeWhile (/='/')) . lines $ contents
		       stripped = map (filter (not . isSpace)) nocomments
		   in  filter (not . null) stripped


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


-- This folding function adds a line of code to an accumulating string

commandsToCodeFF :: SymbolTable -> Command -> String -> String
commandsToCodeFF table (ACommand symb) acc
	      	 | null symb = error "Empty symbol encountered"
	      	 | isDigit (head symb) = ('0' : to15BitFromString symb) ++ ('\n' : acc)
	      	 | M.member symb table = let (Just address) = M.lookup symb table
	      		         	 in ('0' : toKBit 15 "" address) ++ ('\n' : acc)

commandsToCodeFF table (CCommand d c j) acc = let (Just dest) = M.lookup d destMap
	      		         	      	  (Just comp) = M.lookup c compMap
					   	  (Just jump) = M.lookup j jumpMap
				       	      in  ("111" ++ comp ++ dest ++ jump) ++ ('\n' : acc)

commandsToCodeFF table (LCommand l) acc = acc

-- This function turns a list of commands into what we want
commandsToCode :: [Command] -> String
commandsToCode commands =
	        let symbols = getSymbols commands
		in foldr (commandsToCodeFF symbols) "" commands

-- Helper function that converts a positive decimal number into a binary
-- bit string
toKBit :: Int -> String -> Int -> String
toKBit k acc int
       | k <= 0    = acc
       | otherwise = let newbit = int `mod` 2
       	 	     	 rest = int `div` 2
		     in toKBit (k-1) (show newbit ++ acc) rest

to15BitFromString :: String -> String
to15BitFromString strnum = toKBit 15 "" (read strnum)

-- Maps for converting the dest, jump, comp mnemonics into bit strings

destMap :: M.Map String String
destMap = M.fromList [("",    "000"),
	  	      ("M",   "001"),
		      ("D",   "010"),
		      ("MD",  "011"),
		      ("A",   "100"),
		      ("AM",  "101"),
		      ("AD",  "110"),
		      ("AMD", "111")]

compMap :: M.Map String String
compMap = M.fromList [("0",   "0101010"),
	  	      ("1",   "0111111"),
		      ("-1",  "0111010"),
		      ("D",   "0001100"),
		      ("A",   "0110000"),
		      ("M",   "1110000"),
		      ("!D",  "0001101"),
		      ("!A",  "0110001"),
		      ("!M",  "1110001"),
		      ("-D",  "0001111"),
		      ("-A",  "0110011"),
		      ("-M",  "1110011"),
		      ("D+1", "0011111"),
		      ("A+1", "0110111"),
		      ("M+1", "1110111"),
		      ("D-1", "0001110"),
		      ("A-1", "0110010"),
		      ("M-1", "1110010"),
		      ("D+A", "0000010"),
		      ("D+M", "1000010"),
		      ("D-A", "0010011"),
		      ("D-M", "1010011"),
		      ("A-D", "0000111"),
		      ("M-D", "1000111"),
		      ("D&A", "0000000"),
		      ("D&M", "1000000"),
		      ("D|A", "0010101"),
		      ("D|M", "1010101")]

jumpMap :: M.Map String String
jumpMap = M.fromList [("",    "000"),
	  	      ("JGT", "001"),
		      ("JEQ", "010"),
		      ("JGE", "011"),
		      ("JLT", "100"),
		      ("JNE", "101"),
		      ("JLE", "110"),
		      ("JMP", "111")]

-- Here we deal with the symbol table stuff

type SymbolTable = M.Map String Int

-- This helper function adds a variable to a symbol table
-- starting at address 16 as specified in TECS	       

addVarToTable :: (String,Int) -> SymbolTable -> SymbolTable
addVarToTable (var,address) table = M.insert var address table

-- Adds a list of commands to a symbol table and returns the variables
symbolHelper :: [Command] -> Int -> SymbolTable -> [String] -> (SymbolTable,[String])
symbolHelper [] lineNum table vars = (table,vars)
symbolHelper (LCommand l:rest) lineNum table vars = 
	     symbolHelper rest lineNum (M.insert l lineNum table) (delete l vars)

symbolHelper (ACommand a:rest) lineNum table vars
	     | (isDigit . head $ a) || M.member a table || elem a vars =
	        symbolHelper rest (lineNum + 1) table vars
	     | otherwise = symbolHelper rest (lineNum + 1) table (a:vars)

symbolHelper (CCommand d c j:rest) lineNum table vars =
	     symbolHelper rest (lineNum + 1) table vars

-- The preset symbols, not done yet
presetSymbols :: SymbolTable
presetSymbols = M.fromList [("SP",0),
	      		    ("LCL",1),
			    ("ARG",2),
			    ("THIS",3),
			    ("THAT",4),
			    ("R0",0),("R1",1),("R2",2),("R3",3),
			    ("R4",4),("R5",5),("R6",6),("R7",7),
			    ("R8",8),("R9",9),("R10",10),("R11",11),
			    ("R12",12),("R13",13),("R14",14),("R15",15),
			    ("SCREEN",16384),("KBD",24576)]

-- Turns a command list into a symbol table, including the presets
getSymbols :: [Command] -> SymbolTable
getSymbols commands = 
	let (table,vars) = symbolHelper commands 0 presetSymbols []
	    indexedVars = zip (reverse vars) [16..]
	in foldr addVarToTable table indexedVars




