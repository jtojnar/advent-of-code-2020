{-# LANGUAGE NamedFieldPuns #-}
import Data.Map (Map)
import qualified Data.Map as Map

data Instr = Nop | Acc Int | Jmp Int deriving (Show, Eq)

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

next :: Program -> State -> State
next program State {pc, visited, acc} =
    let
        currentInst = Map.findWithDefault Nop pc program
        pc' = case currentInst of
            Jmp n -> pc + n
            _ -> pc + 1
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
findLoop program s = takeWhile (not . previouslyVisited) (iterate (next program) s)

main :: IO ()
main = do
    program <- Map.fromList . zip [0..] . map parseInst . lines <$> readFile "input"
    print (acc (last (findLoop program initialState)))
