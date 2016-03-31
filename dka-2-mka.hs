-- FLP - DKA-2-MKA
-- xvokra00
-- Lukáš Vokráčko

import System.IO
import Text.Printf
import Data.List
import Data.Char
import System.Environment
import Data.Ord
import Data.Function
import qualified Data.Set as Set

data Transition = Transition
	{
		src :: [Char],
		dst :: [Char],
		symbol :: Char
	} deriving (Show, Eq)

data FSM = FSM
	{
		states :: Set.Set[Char],
		transitions :: [Transition],
		startState :: [Char],
		endStates :: Set.Set[Char],
		alphabet :: Set.Set Char,
		sinkAdded :: Bool
	} deriving (Show)

first (x,_,_) = x
second :: ([Char], Int, [(Char, Int)]) -> Int
second (_,x,_) = x
third (_,_,x) = x
secondThird (_,x,y) = (x,y)

-- split string by comma
splitByComma string = split string []
	where
		split [] last = [last]
		split (x:xs) last = if x == ','
			then [last] ++ (split xs [])
			else split xs (last ++ [x])

-- return handle to file or to stdin
getHandle args = do
		if (length args) == 1
			then return stdin
			else openFile (head (tail args)) ReadMode

-- PRINTED ########################################################################################################################################################

-- Print functions for each element of fsm
printListWithSeparator list separator = putStrLn (intercalate separator (map printf list))
printTransitions transitions = mapM (printTransition) transitions
	where
		printTransition transition = putStrLn (printf "%s,%c,%s" (src transition) (symbol transition) (dst transition))

-- print whole fsm
printFsm fsm = do
	printListWithSeparator (Set.toList (states fsm)) ","
	printf "%s\n" (startState fsm)
	printListWithSeparator (Set.toList (endStates fsm)) ","
	printTransitions (transitions fsm)

-- PARSED #########################################################################################################################################################

-- parsing function for each element of fsm
parseStates lines = Set.fromList (splitByComma (lines !! 0)) -- states are on first line separated by comma
parseStartState lines = lines !! 1 -- start state is on second line
parseEndStates lines = Set.fromList (splitByComma (lines !! 2)) -- end states are on third line separated by comma
prepareTransitionLines lines = filter (not . null) (drop 3 lines) -- transitions are on next lines, filter out empty lines
parseTransitions lines = pt lines []
	where
		pt [] transitions = transitions
		pt ([]:_) transitions = transitions
		pt (s:xs) transitions = pt xs ((pt' s):transitions) -- parse one transition and append it to list
		pt' line = Transition -- transition consist of [src, symbol, dst]
			{
				src = (head (splitByComma line)),
				symbol = (head (head (tail (splitByComma line)))),
				dst = (head (tail (tail (splitByComma line))))
			}
parseAlphabet lines = pa lines Set.empty
	where
		pa ([]:_) set = set
		pa lines set = Set.fromList (map (\line -> (splitByComma line) !! 1 !! 0) lines) -- pick symbol from [src, symbol, dst] and insert to set

-- parse  whole fsm
parseFsm lines = FSM
	{
		states = (parseStates lines),
		startState = (parseStartState lines),
		endStates = (parseEndStates lines),
		transitions = (parseTransitions (prepareTransitionLines lines)),
		alphabet = (parseAlphabet (prepareTransitionLines lines)),
		sinkAdded = False
	}

-- WELL DEFINED ###################################################################################################################################################

-- make well defined fsm from fsm (add sink state and transitions to sink for all missingCombinations)
sink fsm = if (missingCombinations fsm) == [] 
	then fsm -- already well defined, adding another sink state would broke it
	else fsm { transitions = ((transitions fsm) ++ (sinkCombinations fsm)), states = Set.insert "" (states fsm), sinkAdded = True } -- add sink state ""
	where
		allCombinations fsm = [(src, symbol) | src <- [""] ++ (Set.toList (states fsm)), symbol <- (Set.toList (alphabet fsm))] -- generate all possible combinations of (src, symbol)
		existingCombinations fsm = map (\tr -> (src tr, symbol tr)) (transitions fsm) -- generate all exesting combinations of (src, symbol)
		missingCombinations fsm = (allCombinations fsm) \\ (existingCombinations fsm) -- find combinations that are missing in fsm
		sinkCombinations fsm = map (\x -> Transition (fst x) "" (snd x)) (missingCombinations fsm) -- construct missing transitions


-- REDUCED ########################################################################################################################################################

ekvClassEnumerate list extract = ekvClassEnumerate' (sort (ekvClassExtract list)) 1
ekvClassEnumerate' [] _ = []
ekvClassEnumerate' (x:xs) number = (ekvClassEnum x number) ++ (ekvClassEnumerate' xs (succ number)) -- enumerate each class 
ekvClassEnum ekvClass number = map (\x -> (x, number, [])) ekvClass -- enumerate states in given class
ekvClassExtract list = map (\ekvclass -> map (\item -> (first item)) ekvclass) (ekvClassGroup list) -- extract states in classes
ekvClassGroup list = groupBy ((==) `on` secondThird) (ekvSort list) -- group states with same class and transition classes
ekvSort list = sortBy (comparing third) (ekvSortSecond list) -- sort states by transitions for grouping
ekvSortSecond list = sortBy (comparing second) (ekvSortTransitions list) --sort states by class
ekvSortTransitions list = map (\(a,b,c) -> (a,b, sortBy (comparing fst) c)) list -- sort transitions of state
ekvClassSink fsm = (show (ekvClass "" (reduce' fsm))) -- get ekvClass for sink state
ekvClass state classes = second (head (filter (\x -> (first x) == state) classes)) -- get ekvClass for given state
ekvClassTransitions classes transitions = map (\x -> Transition { symbol = (symbol x), src = show (ekvClass (src x) classes), dst = show (ekvClass (dst x) classes) }) transitions -- ("A", b, "C") replace with (ekvClass "A", b, ekvclass "C")

stateTransitionClasses state transitions classes = map (\x -> stateTransitionClass x classes) (stateTransitions state transitions) -- replace dst states with its classes
stateTransitionClass transition classes = (symbol transition, second (head (filter (\x -> (first x) == (dst transition)) classes))) -- replace dst state with its class
stateTransitions state transitions = filter (\x -> (src x) == state) transitions -- transition from state

reduceFirstStep fsm = ekvClassEnumerate' [Set.toList (endStates fsm), Set.toList ((states fsm) `Set.difference` (endStates fsm))] 1 -- reduce for end states and non end states
reduce' fsm = reduceStep [] (reduceFirstStep fsm) (transitions fsm) -- determine transitions for states in given classes
reduceStep prevClasses classes transitions = if prevClasses == classes -- until ekvClasses are same
	then classes
	else reduceStep classes (ekvClassEnumerate (reduceStep' classes transitions) True) transitions -- determine transitions, extract new classes
	where
		reduceStep' classes transitions = map (\x -> (first x, second x, stateTransitionClasses (first x) transitions classes)) classes

reduce fsm = fsm -- create reduce fsm
	{
		states = if (sinkAdded fsm)
			then Set.delete (ekvClassSink fsm) (Set.fromList ((map (\x -> show (second x)) (reduce' fsm)))) -- all classes from reduced fsm, sink removed
			else Set.fromList ((map (\x -> show (second x)) (reduce' fsm))), -- all classes from reduced fsm
		startState = show (second (head (filter (\x -> (first x) == (startState fsm)) (reduce' fsm)))), -- class where original start state is
		endStates = Set.fromList (map (\x -> show (second x)) (filter (\x -> (first x) `Set.member` (endStates fsm)) (reduce' fsm))), -- all classes containing original end states
		transitions = if (sinkAdded fsm)
			then filter (\x -> (dst x) /= (ekvClassSink fsm)) (nub (ekvClassTransitions (reduce' fsm) (transitions fsm))) -- all transitions except class where sink is
			else nub (ekvClassTransitions (reduce' fsm) (transitions fsm)) -- all transitions
	}

main = do
	args <- getArgs

	if (length args) > 2 || (length args < 1)
		then error "Invalid argument"
		else do
			handle <- getHandle args
			contents <- hGetContents handle
			let fsm = parseFsm (lines contents)
			case (head args) of
				"-i" -> printFsm fsm
				"-t" -> printFsm (reduce (sink fsm))
				otherwise -> error "Invalid argument"

			hClose handle

