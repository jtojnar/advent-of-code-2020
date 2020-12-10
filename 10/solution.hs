import Data.List (sort)

count :: Bool -> Int
count True = 1
count False = 0

main :: IO ()
main = do
    adapters <- sort . map read . lines <$> readFile "input"
    let differences = zipWith (-) adapters (0:adapters)
    let (threes, ones) = foldr (\n (threes', ones') -> (threes' + count (n == 3), ones' + count (n == 1))) (1, 0) differences
    print (threes * ones)
