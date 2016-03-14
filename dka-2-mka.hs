import System.IO
import Text.Printf
import Data.List
import Data.Char
import System.Environment
import qualified Data.Set as Set

data Transition = Transition
	{
		src :: [Char],
		dst :: [Char],
		symbol :: Char
	} deriving (Show)

data FSM = FSM
	{
		states :: Set.Set[Char],
		transitions :: [Transition],
		startState :: [Char],
		endStates :: Set.Set[Char]
	}

split string = split' string []
	where
		split' [] last = [last]
		split' (x:xs) last = if x == ','
			then [last] ++ (split' xs [])
			else split' xs (last ++ [x])

printJoinList list separator = putStrLn (intercalate separator (map printf list))
printTransitions ts = mapM (printTransition) ts
printTransition t = putStrLn (printf "%s,%c,%s" (src t) (symbol t) (dst t))
printFsm fsm = do
	printJoinList (Set.toList (states fsm)) ","
	printf "%s\n" (startState fsm)
	printJoinList (Set.toList (endStates fsm)) ","
	printTransitions (transitions fsm)

parseStates lines = Set.fromList (split (lines !! 0))
parseStartStates lines = split (lines !! 1) !! 0
parseEndStates lines = Set.fromList (split (lines !! 2))
parseTransitions lines = pt lines []
	where
		pt [] transitions = transitions
		pt (s:xs) transitions = pt xs ((pt' s):transitions)
		pt' line = Transition
			{
				src = (head (split line)),
				symbol = (head (head (tail (split line)))),
				dst = (head (tail (tail (split line))))
			}

parseFsm lines = FSM
	{
		states = (parseStates lines),
		startState = (parseStartStates lines),
		endStates = (parseEndStates lines),
		transitions = (parseTransitions (drop 3 lines))
	}

minimalize fsm = reduce (removeUnreachable fsm)
reduce fsm = fsm
removeUnreachable fsm = FSM
	{
		states = reachable (startState fsm) (transitions fsm),
		startState = (startState fsm),
		endStates = (endStates fsm) `Set.intersection` (reachable (startState fsm) (transitions fsm)),
		transitions = reachableTransitions (transitions fsm) (reachable (startState fsm) (transitions fsm))
	}
reachable start transitions = reachable' (Set.empty) (Set.singleton start) transitions
	where
		reachable' prev curr transitions = if prev == curr
			then curr
			else reachable' curr (step curr transitions) transitions
		step curr transitions = (Set.fromList (map (dst) (filter (\x -> (src x) `Set.member` curr) transitions))) `Set.union` curr
reachableTransitions transitions states = filter (\x -> (src x) `Set.member` states) transitions

main = do
	args <- getArgs

	if (length args) /= 2
		then error "Invalid argument"
		else do
			handle <- openFile (head (tail args)) ReadMode
			contents <- hGetContents handle
			let fsm = parseFsm (lines contents)
			case (head args) of
				"-i" -> printFsm fsm
				"-t" -> printFsm (minimalize fsm)
				otherwise -> error "Invalid argument"

			hClose handle
