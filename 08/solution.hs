{-# LANGUAGE NamedFieldPuns #-}
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Instr = Nop Int | Acc Int | Jmp Int deriving (Show, Eq)

type Program = Map Int Instr

data State = State {
    pc :: Int,
    visited :: Map Int Bool,
    acc :: Int
} deriving (Show, Eq)

initialState :: State
initialState = State 0 Map.empty 0

parseSig :: Char -> Int
parseSig '+' = 1
parseSig '-' = -1

parseInst :: String -> Instr
parseInst ('n':'o':'p':' ':sig:n) = Nop (parseSig sig * read n)
parseInst ('j':'m':'p':' ':sig:n) = Jmp (parseSig sig * read n)
parseInst ('a':'c':'c':' ':sig:n) = Acc (parseSig sig * read n)

previouslyVisited :: State -> Bool
previouslyVisited State {pc, visited} = Map.findWithDefault False pc visited

flipInstr :: Instr -> Instr
flipInstr (Nop n) = Jmp n
flipInstr (Jmp n) = Nop n
flipInstr i = i

nextPc :: Instr -> Int -> Int
nextPc (Jmp n) pc = pc + n
nextPc _ pc = pc + 1

next :: Program -> Set Int -> State -> State
next program alternateNextLinesToFlipTo State {pc, visited, acc} =
    let
        currentInst =
            let
                originalInstr = Map.findWithDefault (Nop 0) pc program
                alternateInstr = flipInstr originalInstr
                shouldFlip =
                    originalInstr /= alternateInstr &&
                    -- Do not flip again once terminating state is reachable.
                    not (nextPc originalInstr pc `Set.member` alternateNextLinesToFlipTo) &&
                    nextPc alternateInstr pc `Set.member` alternateNextLinesToFlipTo
            in
                if shouldFlip then alternateInstr else originalInstr

        pc' = nextPc currentInst pc
        acc' = case currentInst of
            Acc n -> acc + n
            _ -> acc
        state' = State {
            pc = pc',
            acc = acc',
            visited = Map.insert pc True visited
        }
    in
        state'

findLoop :: Program -> State -> [State]
findLoop program s = takeWhile (not . previouslyVisited) (iterate (next program Set.empty) s)

findReach :: Program -> Int -> State -> Maybe State
findReach program target s =
    let
        terminatingLines = reachable (invertGraph (toGraph program)) target
    in
        find (\State {pc} -> pc == target) (iterate (next program terminatingLines) s)

toGraph :: Program -> Map Int [Int]
toGraph = Map.mapWithKey (\lineNo instr -> [nextPc instr lineNo])

invertGraph :: Map Int [Int] -> Map Int [Int]
invertGraph = Map.foldrWithKey (\from tos g -> foldr (\to g' -> Map.insertWith (\_ old -> from:old) to [from] g') g tos) Map.empty

reachable :: Map Int [Int] -> Int -> Set Int
reachable graph initial =
    let
        next_ state = Set.fromList (Map.findWithDefault [] state graph)

        reachable' explored boundary | Set.null boundary = explored
        reachable' explored boundary =
            let
                nexts = Set.unions (Set.map next_ boundary)
                explored' = Set.union explored boundary
                boundary' = Set.difference nexts explored'
            in
                reachable' explored' boundary'
    in
        reachable' Set.empty (Set.singleton initial)

main :: IO ()
main = do
    program <- Map.fromList . zip [0..] . map parseInst . lines <$> readFile "input"
    print (acc (last (findLoop program initialState)))
    print (findReach program (Map.size program) initialState)
