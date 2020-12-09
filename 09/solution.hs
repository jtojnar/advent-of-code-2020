import Data.List (find)

takeWhileChanges :: Eq a => [a] -> [a]
takeWhileChanges xs = map fst (takeWhile (uncurry (/=)) (zip xs (drop 1 xs)))

firstValids :: Int -> [Int] -> [(Int, [Int])]
firstValids windowSize numbers =
    let
        preamble = take windowSize numbers
    in
        [ (a, [ a + b | b <- drop k preamble ]) | (a, k) <- zip preamble [1..] ]

nextValids :: Int -> ([(Int, [Int])], [Int]) -> ([(Int, [Int])], [Int])
nextValids windowSize (window, []) = (window, []) -- cycle when reaching end
nextValids windowSize ([], _) = error "window must be non-empty"
nextValids windowSize (_:windowValidsTail, newNum:numbers') =
    let
        windowValids' = map (\(num, valids) -> (num, (num + newNum):valids)) windowValidsTail ++ [(newNum, [])]
    in
        (windowValids', numbers')

computeValids :: Int -> [Int] -> [([(Int, [Int])], [Int])]
computeValids windowSize numbers = takeWhileChanges (iterate (nextValids windowSize) (firstValids windowSize numbers, drop windowSize numbers))

findInvalid :: [([(Int, [Int])], [Int])] -> Maybe ([(Int, [Int])], [Int])
findInvalid =
    let
        getValids = concatMap snd
    in
        find (\(windowValids, num:numbers') -> not (num `elem` getValids windowValids))

main :: IO ()
main = do
    numbers <- map read . lines <$> readFile "input"
    print (fmap (head . snd) (findInvalid (computeValids 25 numbers)))
