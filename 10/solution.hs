import Data.List (sort)

count :: Bool -> Int
count True = 1
count False = 0

valid :: [Int] -> Int -> Int
valid adapters =
    let
        valid' 0 = 1
        valid' n | n < 0 = 0
        valid' n = if n `elem` adapters then valid'' (n-1) + valid'' (n-2) + valid'' (n-3) else 0
        valid'' n | n >= 2 = valids !! n
        valid'' n = valid' n
        valids = map valid' [0..]
    in
        valid'

main :: IO ()
main = do
    adapters <- sort . map read . lines <$> readFile "input"
    let device = maximum adapters + 3
    let adapters' = adapters ++ [device]
    let differences = zipWith (-) adapters' (0:adapters')
    let (threes, ones) = foldr (\n (threes', ones') -> (threes' + count (n == 3), ones' + count (n == 1))) (0, 0) differences
    print (threes * ones)
    print (valid adapters' device)
