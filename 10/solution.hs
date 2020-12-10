import Data.List (sort)

count :: Bool -> Int
count True = 1
count False = 0

valid :: [Int] -> Int -> Int
valid adapters 0 = 1
valid adapters n = if n `elem` adapters then valid adapters (n-1) + valid adapters (n-2) + valid adapters (n-3) else 0

main :: IO ()
main = do
    adapters <- sort . map read . lines <$> readFile "input"
    let device = maximum adapters + 3
    let adapters' = adapters ++ [device]
    let differences = zipWith (-) adapters' (0:adapters')
    let (threes, ones) = foldr (\n (threes', ones') -> (threes' + count (n == 3), ones' + count (n == 1))) (0, 0) differences
    print (threes * ones)
    print (valid adapters' device)
