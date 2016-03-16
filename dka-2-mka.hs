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
	} deriving (Show)

data FSM = FSM
	{
		states :: Set.Set[Char],
		transitions :: [Transition],
		startState :: [Char],
		endStates :: Set.Set[Char],
		alphabet :: Set.Set Char
	}

third (_,_,x) = x
second (_,x,_) = x
first (x,_,_) = x
secondThird (_,x,y) = (x,y)
-- split string by comma
splitByComma string = split string []
	where
		split [] last = [last]
		split (x:xs) last = if x == ','
			then [last] ++ (split xs [])
			else split xs (last ++ [x])

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
		alphabet = (parseAlphabet (prepareTransitionLines lines))
	}

-- make well defined fsm from fsm (add sink state and transitions to sink for all missingCombinations)
sink fsm = fsm { transitions = ((transitions fsm) ++ (sinkCombinations fsm)) }
	where
		allCombinations fsm = [(src, symbol) | src <- (Set.toList (states fsm)), symbol <- (Set.toList (alphabet fsm))] -- generate all possible combinations of (src, sybol)
		existingCombinations fsm = map (\tr -> (src tr, symbol tr)) (transitions fsm) -- generate all exesting combinations of (src, symbol)
		missingCombinations fsm = (allCombinations fsm) \\ (existingCombinations fsm) -- find combinations that are missing in fsm
		sinkCombinations fsm = map (\x -> Transition (fst x) "" (snd x)) (missingCombinations fsm) -- construct missing transitions

-- remove unreachable states and associated transitions from fsm
removeUnreachable fsm = fsm
	{
		states = reachable (startState fsm) (transitions fsm),
		endStates = (endStates fsm) `Set.intersection` (reachable (startState fsm) (transitions fsm)),
		transitions = reachableTransitions (transitions fsm) (reachable (startState fsm) (transitions fsm))
	}
	where
		reachable start transitions = reachable' (Set.empty) (Set.singleton start) transitions -- start state is always reachable
		reachable' prev curr transitions = if prev == curr 
			then curr -- if no more reachable states have been added stop
			else reachable' curr (step curr transitions) transitions -- else find reachle states from currenly reachable
		step curr transitions = (Set.fromList (map (dst) (filter (\x -> (src x) `Set.member` curr) transitions))) `Set.union` curr -- new reachable states
		reachableTransitions transitions states = filter (\x -> (src x) `Set.member` states) transitions -- filter only transition from reachable states


-- TODO !! dokončit ekvivalentní třídy

stateTransitions state transitions classes = filter (\x -> (src x) == state) transitions
stateTransitionClasses transitions classes = map (\x -> stateClass x classes) transitions
stateClass transition classes = (symbol transition, second (head (filter (\x -> (first x) == (dst transition)) classes)))
-- expects result of one step decomposition, returns classes for next step
classListEnum classList = cLE (classExtract classList) 0
cLE [] _ = []
cLE (x:xs) number = (classEnum x number) ++ (cLE xs (succ number))
classGroup list = groupBy ((==) `on` secondThird) (sortBy (comparing third) list) -- group by same classes for dst
classExtract list = map (\ekvclass -> map (\item -> (first item)) ekvclass) (classGroup list) -- extract states in classes
classEnum ekvclass number = map (\x -> (x, number, [])) ekvclass -- enumerate states in class

minimalize fsm = reduce (sink (removeUnreachable fsm))
reduce fsm = fsm
reduceFirstStep fsm = cLE [Set.toList (endStates fsm), Set.toList ((states fsm) `Set.difference` (endStates fsm))] 0
-- reduceStep fsm = map (\x -> constructTransitions (first x) (second x) classes (filterTransitions (first x) (transitions fsm))) states
-- 	where 
-- 		filterTransitions state transitions = filter (\x -> (src x) == state) transitions
-- 		constructTransitions state id classes transitions = (state, id, map (\x -> (symbol x, determineClass (dst x) classes)) transitions)
-- 		determineClass state classes = second (filter (\x -> (first x) == state) classes)
-- ekvivalentClasses states endStates transitions = ekv [(0, endStates), (1, states `Set.intersection` endStates)] transitions
-- 	where
-- 		ekv =


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

y = [("B",1,[('a',1),('b',1)]), ("C", 0, [('a', 1), ('b',0)]), ("D", 1, [('a',1),('b',0)]), ("E", 1, [('a',1),('b',1)])]
