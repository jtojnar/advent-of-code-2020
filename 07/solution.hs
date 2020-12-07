import Control.Applicative ((<|>), liftA3)
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP

type Color = String

word :: ReadP String
word = munch1 (not . isSpace)

color :: ReadP Color
color = (\adj sep col -> adj <> sep <> col) <$> word <*> string " " <*> word

number :: ReadP Int
number = read <$> munch1 isDigit

bagNumber :: ReadP (Color, Int)
bagNumber = flip (,) <$> number <*> (string " " *> color <* string " bag" <* optional (string "s"))

contents :: ReadP [(Color, Int)]
contents = fmap (const []) (string "no other bags") <|> sepBy1 bagNumber (string ", ")

rule :: ReadP (Color, [(Color, Int)])
rule = (,) <$> (color <* string " bags contain ") <*> (contents <* string ".")

parseRule :: String -> (Color, [(Color, Int)])
parseRule line =
    case readP_to_S rule line of
        [(rule, "")] -> rule
        _ -> error ("Unable to parse line: " <> line)

toGraph :: [(Color, [(Color, Int)])] -> Map Color [(Color, Int)]
toGraph = Map.fromList

invertGraph :: Map Color [(Color, Int)] -> Map Color [(Color, Int)]
invertGraph = Map.foldrWithKey (\from tos g -> foldr (\(to, label) g' -> Map.insertWith (\_ old -> (from, label):old) to [(from, label)] g') g tos) Map.empty

reachable :: Map Color [(Color, Int)] -> Color -> Set Color
reachable graph initial =
    let
        next color = Set.fromList (map fst (Map.findWithDefault [] color graph))

        reachable' explored boundary | Set.null boundary = explored
        reachable' explored boundary =
            let
                nexts = Set.unions (Set.map next boundary)
                explored' = Set.union explored boundary
                boundary' = Set.difference nexts explored'
            in
                reachable' explored' boundary'
    in
        reachable' Set.empty (Set.singleton initial)

traverseAndCount :: Map Color [(Color, Int)] -> Color -> Int
traverseAndCount graph =
    let
        traverseAndCount' color =
            1 + sum (map (\(c, n) -> n * traverseAndCount' c) (Map.findWithDefault [] color graph))
    in
        traverseAndCount'

main :: IO ()
main = do
    rules <- toGraph . fmap parseRule . lines <$> readFile "input"
    let invertedRules = invertGraph rules
    print (Set.size (reachable invertedRules "shiny gold") - 1)
    print (traverseAndCount rules "shiny gold" - 1)
