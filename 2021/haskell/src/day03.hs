import AoC

-- import Debug.Trace ( trace )

main :: IO ()
main = do
    example <- readFile $ getExample 3
    input   <- readFile $ getInput   3

    let exa = lines example
    let inp = lines input

    let solve1 = powerConsumption . epsGam
    print $ "Part1 example: " ++ show (solve1 $ exa)
    print $ "Part1 input  : " ++ show (solve1 $ inp)

    let apply f (a,b) c = (f a c, f b c)
    let oxyCarb = apply part2 ((>=), (<))
    -- print $ show $ oxyCarb exa
    print $ "Part2 example: " ++ show (powerConsumption $ oxyCarb exa)
    print $ "Part2 input  : " ++ show (powerConsumption $ oxyCarb inp)
    -- 2556658 TOO HIGH

-- Function `criteria` compares the number of '1s' with half the number of (remaining) bits
-- e.g. 5 1s in 7bytes => 5 `criteria` (ceil 5/2)
part2 :: (Int -> Int -> Bool) -> [[Char]] -> [Char]
part2 criteria bytes =
    let num1At i bs = length $ filter (\b -> '1' == getBitAt i b) bs
        domBit i bs = if (criteria (num1At i bs) (b + (mod a b))) then '1' else '0'
            where
                a = length bs
                b = div a 2
        search i bs 
            | length bs == 1 = bs
            | otherwise      = search (i+1) $ filter (\byte -> db == getBitAt i byte) bs
            where db = domBit i bs
    in head $ search 0 bytes

powerConsumption :: (String, String) -> Int
powerConsumption (left, right) = (unbinary left) * (unbinary right)

epsGam :: [[Char]] -> ([Char], [Char])
epsGam bytes =
    let dominant    = map (\n -> dominantBit n bytes) [0..byteLen bytes]
        most        = foldl (\(eps, gam) b -> if b == '1' then (eps ++ "1", gam ++ "0") else (eps ++ "0", gam ++ "1")) ("", "")
    in most dominant

dominantBit :: Int -> [[Char]] -> Char
dominantBit n bytes = 
    let bits = map (getBitAt n) bytes
        num1 = length $ filter (\x -> x == '1') bits
    in if num1 >= (length bytes `div` 2)
        then '1'
        else '0'

getBitAt :: Int -> [Char] -> Char
getBitAt n = head . drop n

byteLen :: [[Char]] -> Int
byteLen = (subtract 1) . length . head

unbinary :: String -> Int
unbinary = unbinary' 0 1 . reverse
    where
        unbinary' :: Int -> Int -> [Char] -> Int
        unbinary' acc _ [] = acc
        unbinary' acc n txt = 
            let (x:xs)  = txt
                nextAcc = acc + n * (if x == '1' then 1 else 0)
            in unbinary' nextAcc (n*2) xs