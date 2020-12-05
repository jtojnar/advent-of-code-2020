import Data.List (sort)

codeToPlace :: String -> (Int, Int)
codeToPlace code =
    let
        parseRow n 'F' = n * 2
        parseRow n 'B' = n * 2 + 1
        parseColumn n 'L' = n * 2
        parseColumn n 'R' = n * 2 + 1
    in
        (foldl parseRow 0 (take 7 code), foldl parseColumn 0 (drop 7 code))

placeToId :: (Int, Int) -> Int
placeToId (row, col) = row * 8 + col

main :: IO ()
main = do
    seats <- map (placeToId . codeToPlace) . lines <$> readFile "input"
    print (maximum seats)
    print (let sortedSeats = sort seats in map (\(a, b) -> a + 1) (filter (\(a, b) -> b /= a + 1) (zip sortedSeats (drop 1 sortedSeats))))
