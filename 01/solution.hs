findTwo :: [Int] -> [Int]
findTwo numbers = [x*y | x <- numbers, y <- numbers, x+y == 2020]

findThree :: [Int] -> [Int]
findThree numbers = [x*y*z | x <- numbers, y <- numbers, z <- numbers, x+y+z == 2020]

main :: IO ()
main = do
    numbers <- map read . lines <$> readFile "01/input"
    print (findTwo numbers)
    print (findThree numbers)
