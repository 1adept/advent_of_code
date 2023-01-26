import AoC
import Data.List.Split ( splitOn )

data Range = Range Int Int
    deriving (Show)
data Assignment = Assignment Range Range
    deriving (Show)

example :: String
example ="2-4,6-8\n\
\2-3,4-5\n\
\5-7,7-9\n\
\2-8,3-7\n\
\6-6,4-6\n\
\2-6,4-8\n";

main :: IO ()
main = do
    content <- readFile $ getInput 4

    let ass         = map parseAssignment $ lines content
        count f     = length $ filter f $ ass
        fullAss     = count fullyContained
        partialAss  = count partiallyContained

    putStrLn $ "Ranges in assignments fully contained: " ++ show fullAss
    putStrLn $ "Ranges in assignments partially contained: " ++ show partialAss

contained :: Assignment -> (Range -> Range -> Bool) -> Bool
contained (Assignment a b) f = f a b || f b a

partiallyContained :: Assignment -> Bool
partiallyContained ass = contained ass rangePartiallyContained

fullyContained :: Assignment -> Bool
fullyContained ass = contained ass rangeFullContained

rangePartiallyContained :: Range -> Range -> Bool
rangePartiallyContained (Range a1 a2) (Range b1 b2) = a1 <= b1 && a2 >= b1 || a1 <= b2 && a2 >= b2

rangeFullContained :: Range -> Range -> Bool
rangeFullContained (Range a1 a2) (Range b1 b2) = a1 <= b1 && a2 >= b2

parseAssignment :: String -> Assignment
parseAssignment str = Assignment a b
    where [a, b] = map parseRange $ splitOn "," str

parseRange :: String -> Range
parseRange str = 
    let [from, to] = map read $ splitOn "-" str
    in Range from to