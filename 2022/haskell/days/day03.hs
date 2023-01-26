import AoC
import Data.List
import Data.Char

example :: String
example = "vJrwpWtwJgWrhcsFMMfFFhFp\n\
\jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
\PmmdzqPrVvPwwTWBwg\n\
\wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
\ttgJtRGJQctTZtZT\n\
\CrZsJsPPZsGzwwsLwLmpwMDw"


main :: IO ()
main = do
    content <- readFile $ getInput 3
    let rucksacks = fmap parseRucksack $ lines content

    print $ part1 rucksacks
    print $ part2 rucksacks

    putStrLn "End Day 03"

part1 :: [(String, String)] -> Int
part1 = sum . fmap (priority . commonItem)

part2 :: [(String, String)] -> Int
part2 rucksacks = sum $ fmap priority badges
    where
        groups = group3 rucksacks
        badges = fmap badgeOf groups

badgeOf :: [(String, String)] -> Char
badgeOf rucksacks = head $ intersect a $ intersect b c
    where [a, b, c] = fmap merge rucksacks
    

merge :: (String, String) -> String
merge (left, right) = left ++ right

group3 :: [a] -> [[a]]
group3 [] = []
group3 rucksacks = [a,b,c] : group3 rest
    where
        (a:b:c:rest) = rucksacks

parseRucksack :: String -> (String, String)
parseRucksack str = splitAt ((length str) `div` 2) str
    
commonItem :: (String, String) -> Char
commonItem (left, right) = head $ intersect left right

priority :: Char -> Int
priority item 
    | isLower item  = ord item - ord 'a' + 1
    | otherwise     = ord item - ord 'A' + 27