import Control.Applicative (liftA2)
import Data.List ((\\))
import Data.Char (isDigit, isHexDigit)

requiredFields :: [String]
requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

clusterPassports :: [String] -> [[String]]
clusterPassports =
    let
        addPassport "" ps = []:ps
        addPassport line [] = [[line]]
        addPassport line (p:ps) = (p ++ words line):ps
    in
        foldr addPassport []

requiredFieldsPresent :: [String] -> Bool
requiredFieldsPresent = null . (requiredFields \\) . map (take 3)

fieldsValid :: [String] -> Bool
fieldsValid =
    let
        valid ('b':'y':'r':':':val) = all isDigit val && read val >= 1920 && read val <= 2002
        valid ('i':'y':'r':':':val) = all isDigit val && read val >= 2010 && read val <= 2020
        valid ('e':'y':'r':':':val) = all isDigit val && read val >= 2020 && read val <= 2030
        valid ('h':'g':'t':':':val) =
            case reads val of
                [(n, "cm")] -> n >= 150 && n <= 193
                [(n, "in")] -> n >= 59 && n <= 76
                _ -> False
        valid ('h':'c':'l':':':'#':val) = all isHexDigit val && length val == 6
        valid ('e':'c':'l':':':val) = val `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        valid ('p':'i':'d':':':val) = all isDigit val && length val == 9
        valid ('c':'i':'d':_) = True
        valid _ = False
    in
        all valid

main :: IO ()
main = do
    passports <- clusterPassports . lines <$> readFile "input"
    print (length (filter id (map requiredFieldsPresent passports)))
    print (length (filter id (map (liftA2 (&&) requiredFieldsPresent fieldsValid) passports)))
