import System.IO
import Text.Printf
import Data.List
import Data.Char
import System.Environment

data Transition = Transition
	{
		src :: [Char],
		dst :: [Char],
		symbol :: Char
	} 

data FSM = FSM
	{
		states :: [[Char]],
		transitions :: [Transition],
		startState :: [Char],
		endStates :: [[Char]]
	}

printJoinList list separator = putStrLn (intercalate separator (map printf list))
printTransitions ts = mapM (printTransition) ts
printTransition t = putStrLn (printf "%s,%c,%s" (src t) (symbol t) (dst t))

printFsm fsm = do
	printJoinList (states fsm) ","
	printf "%s\n" (startState fsm)
	printJoinList (endStates fsm) ","
	printTransitions (transitions fsm)

split string = split' string []
	where 
		split' [] last = [last]
		split' (x:xs) last = if x == ',' 
			then [last] ++ (split' xs []) 
			else split' xs (last ++ [x])

parseStates lines = split (lines !! 0) 
parseStartStates lines = split (lines !! 1) !! 0
parseEndStates lines = split (lines !! 2) 
parseTransitions lines = pt lines []
	where 
		pt [] transitions = transitions
		pt (s:xs) transitions = pt xs ((pt' s):transitions)
		pt' line = Transition {src = (head (split line)), symbol = (head (head (tail (split line)))), dst = (head (tail (tail (split line))))}
--
parseFsm lines = FSM {states = (parseStates lines), startState = (parseStartStates lines), endStates = (parseEndStates lines), transitions = (parseTransitions (drop 3 lines))}

minimalizeFsm fsm = fsm

main = do
	args <- getArgs

	if (length args) /= 2
		then error "Invalid argument"
		else do
			handle <- openFile (head (tail args)) ReadMode
			contents <- hGetContents handle
			let fsm = parseFsm (lines contents)
    --
			case (head args) of
				"-i" -> printFsm fsm
				"-t" -> printFsm (minimalizeFsm fsm)
				otherwise -> error "Invalid argument"

			hClose handle
