parseLine :: String -> (Int, Int, Char, String)
parseLine line =
    let
        [(low, '-':rest)] = reads line
        [(high, ' ':rest')] = reads rest
        c:':':' ':pass = rest'
    in
        (low, high, c, pass)

lineValid :: (Int, Int, Char, String) -> Bool
lineValid (low, high, c, pass) =
    let
        occurences = length (filter (== c) pass)
    in
        occurences >= low && occurences <= high

lineValidNew :: (Int, Int, Char, String) -> Bool
lineValidNew (low, high, c, pass) = (pass !! (low - 1) == c) /= (pass !! (high - 1) == c)

main :: IO ()
main = do
    passwords <- map parseLine . lines <$> readFile "input"
    print (length (filter lineValid passwords))
    print (length (filter lineValidNew passwords))
