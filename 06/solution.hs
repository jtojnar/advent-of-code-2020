import Data.List (sort)

clusterGroups :: [String] -> [[String]]
clusterGroups =
    let
        addPerson "" groups = []:groups
        addPerson line [] = [[sort line]]
        addPerson line (group:groups) = (sort line:group):groups
    in
        foldr addPerson []

countYeses :: ([Bool] -> Bool) -> [String] -> Int
countYeses method =
    let
        newLetter l (x:xs) | l == x = (True, xs)
        newLetter _ xxs = (False, xxs)

        countYeses' letter yesQuestions persons =
            if all null persons then
                yesQuestions
            else
                let
                    (yeses, persons') = unzip (map (newLetter letter) persons)
                    yesQuestions' = if method yeses then yesQuestions + 1 else yesQuestions
                in
                    countYeses' (succ letter) yesQuestions' persons'
    in
        countYeses' 'a' 0

main :: IO ()
main = do
    groups <- clusterGroups . lines <$> readFile "input"
    print (sum (map (countYeses or) groups))
    print (sum (map (countYeses and) groups))
