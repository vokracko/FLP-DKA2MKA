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
-- split string by comma
split string = split' string []
	where
		split' [] last = [last]
		split' (x:xs) last = if x == ','
			then [last] ++ (split' xs [])
			else split' xs (last ++ [x])

-- Print functions for each element of fsm
printJoinList list separator = putStrLn (intercalate separator (map printf list))
printTransitions ts = mapM (printTransition) ts
printTransition t = putStrLn (printf "%s,%c,%s" (src t) (symbol t) (dst t))

-- print whole fsm 
printFsm fsm = do
	printJoinList (Set.toList (states fsm)) ","
	printf "%s\n" (startState fsm)
	printJoinList (Set.toList (endStates fsm)) ","
	printTransitions (transitions fsm)

-- parsing function for each element of fsm
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
parseAlphabet lines = pa lines Set.empty
	where 
		pa lines set = Set.fromList (map (\line -> (split line) !! 1 !! 0) lines)

-- parse  whole fsm
parseFsm lines = FSM
	{
		states = (parseStates lines),
		startState = (parseStartStates lines),
		endStates = (parseEndStates lines),
		transitions = (parseTransitions (drop 3 lines)),
		alphabet = (parseAlphabet (drop 3 lines))
	}

stateTransitions state transitions classes = filter (\x -> (src x) == state) transitions
stateTransitionClasses transitions classes = map (\x -> stateClass x classes) transitions
stateClass transition classes = (symbol transition, second (head (filter (\x -> (first x) == (dst transition)) classes)))
-- expects result of one step decomposition, returns classes for next step
classListEnum classList = cLE (classExtract classList) 0
	where
		cLE [] _ = []
		cLE (x:xs) number = (classEnum x number) ++ (cLE xs (succ number)) 
		classGroup list = groupBy ((==) `on` third) (sortBy (comparing third) list) -- group by same classes for dst
		classExtract list = map (\ekvclass -> map (\item -> (first item)) ekvclass) (classGroup list) -- extract states in classes
		classEnum ekvclass number = map (\x -> (x, number, [])) ekvclass -- enumerate states in class

minimalize fsm = reduce (sink (removeUnreachable fsm))
-- sink fsm = fsm
sink fsm = fsm { transitions = ((transitions fsm) ++ (sinkCombinations fsm)) }
	where
		allCombinations fsm = [(src, symbol) | src <- (Set.toList (states fsm)), symbol <- (Set.toList (alphabet fsm))]
		existingCombinations fsm = map (\tr -> (src tr, symbol tr)) (transitions fsm)
		missingCombinations fsm = (allCombinations fsm) \\ (existingCombinations fsm)
		sinkCombinations fsm = map (\x -> Transition (fst x) "" (snd x)) (missingCombinations fsm)
reduce fsm = fsm
	-- {
	-- 	states =
	-- 	startState =
	-- 	endStates = 
	-- 	transitions =
	-- }

-- ekvivalentClasses states endStates transitions = ekv [(0, endStates), (1, states `Set.intersection` endStates)] transitions
-- 	where 
-- 		ekv = 
removeUnreachable fsm = fsm
	{
		states = reachable (startState fsm) (transitions fsm),
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

y = [("B",1,[('a',1),('b',1)]), ("C", 1, [('a', 1), ('b',0)]), ("D", 1, [('a',1),('b',0)]), ("E", 1, [('a',1),('b',1)])]
