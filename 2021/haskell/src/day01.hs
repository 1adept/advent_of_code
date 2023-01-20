import AoC

main :: IO ()
main = do 
    example <- readFile $ getExample 1
    input <- readFile $ getInput 1

    let measurementsEx = map readMeasurement $ lines example
    let measurementsIn = map readMeasurement $ lines input

    let showCount = show . countIncreases
    print $ "Part1 Example: " ++ (showCount measurementsEx)
    print $ "Part1 Input: " ++ (showCount measurementsIn)
    print $ "---------------------"
    print $ "Part2 Example: " ++ (showCount $ group3 [] measurementsEx)
    print $ "Part2 Input: " ++ (showCount $ group3 [] measurementsIn)

countIncreases :: [Int] -> Int 
countIncreases measure =
    let zipped = zip measure (drop 1 measure)
        filtered = filter (\(x, y) -> y > x) zipped
    in length filtered

readMeasurement :: String -> Int
readMeasurement = read

group3 :: [Int] -> [Int] -> [Int]
group3 acc readings  
    | length readings < 3 = acc
    | otherwise           = group3 (acc ++ [sum3 readings]) (drop 1 readings)
    where 
        sum3 :: [Int] -> Int  
        sum3 = sum <$> take 3 