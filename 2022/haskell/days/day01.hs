import Data.List
import Data.String
import System.Environment

input :: String
input = "1000\n\
\2000\n\
\3000\n\
\\n\
\4000\n\
\\n\
\5000\n\
\6000\n\
\\n\
\7000\n\
\8000\n\
\9000\n\
\\n\
\10000\n"

main :: IO ()
main = do
    args <- getArgs
    let input = head args
    let calories = getCalories input
    let elves = mapElvesInt $ getElves calories
    let elvesSum = map sumElve elves
    let maxElve = maximum' elvesSum
    putStrLn $ "Max elve " ++ show maxElve
    putStrLn $ 
        "Top three elves have " ++ show (getTopX 3 elvesSum) 
        ++ " with sum " ++ show (sum $ getTopX 3 elvesSum)

getCalories :: String -> [String]
getCalories input = lines input

getElves :: [String] -> [[String]]
getElves [] = []
getElves x = takeWhile (/= []) x : (getElves $ drop 1 $ dropWhile (/= []) (x))

mapElvesInt :: [[String]] -> [[Int]]
mapElvesInt [] = []
mapElvesInt cals = [map toInt x | x <- cals]

toInt :: String -> Int
toInt i = read i :: Int

sumElve :: [Int] -> Int
sumElve [] = 0
sumElve elve = sum elve

maximum' :: [Int] -> Int
maximum' [] = 0
maximum' [x] = x
maximum' (x:xs) 
    | m > x     = m
    | otherwise = x
    where m = maximum' xs

getTopX :: Int -> [Int] -> [Int]
getTopX n x = take n $ reverse $ sort x