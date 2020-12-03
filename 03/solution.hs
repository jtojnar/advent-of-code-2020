move :: Int -> Int -> [[Char]] -> [[Char]]
move x y field = map (drop x) (drop y field)

tl :: [[Char]] -> Char
tl = head . head

notNull :: [a] -> Bool
notNull = fmap not null

countTrees :: [[Char]] -> Int -> Int -> Int
countTrees field x y = length (filter (=='#') (map tl (takeWhile notNull (iterate (move x y) field))))

main :: IO ()
main = do
    field <- map cycle . lines <$> readFile "input"
    print (countTrees field 3 1)
    print (product (map (uncurry (countTrees field)) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]))
