import AoC

type Instruction = Maybe Int
type State       = (Int, Int)   -- (Cycle, Register value)

day :: Int
day = 10

myEx :: IO [Instruction]
myEx = do
    example <- readFile $ getExample day
    return $ parse example
myIn :: IO [Instruction]
myIn = do
    input <- readFile $ getInput day
    return $ parse input

main :: IO ()
main = do
    example <- readFile $ getExample day
    input   <- readFile $ getInput day

    let e = parse example
    let i = parse input

    print $ "Part1 example: " ++ show (solve1 e)
    print $ "Part1   input: " ++ show (solve1 i)

    -- Doesnt work with this uncommented
    -- putStrLn "Part2 example:"
    -- printCRT $ solve2 e

    putStrLn "Part2 input:"
    printCRT $ solve2 i

    where
        printCRT :: [String] -> IO ()
        printCRT []           = return ()
        printCRT (first:rest) = do
            putStrLn first
            printCRT rest

solve1 :: [Instruction] -> Int
solve1 = sum 
        . map (uncurry (*))
        . filter (\(cycle, _) -> cycle >= 20 && (cycle - 20) `mod` 40 == 0) 
        . exec

solve2 :: [Instruction] -> [String]
solve2 instructions = breakLines [] $ spritify instructions
    where
        spritify :: [Instruction] -> String
        spritify = 
                map (
                    (\b -> if b then '#' else '.')
                    . (\(c, r) -> abs (((c - 1) `mod` 40) - r) <= 1)
                )
                . exec
        breakLines :: [String] -> String -> [String]
        breakLines res []  = res
        breakLines res src = breakLines (res ++ [take 40 src]) (drop 40 src)

exec :: [Instruction] -> [State]
exec instructions = exec' instructions [(1, 1)] -- Starting cycle and register value are 1 each
    where
        exec' :: [Instruction] -> [State] -> [State]
        exec' []     result = reverse result
        exec' (i:is) result@(last:_) = 
            let (lastCycle, lastReg) = last
                next = (lastCycle + 1, lastReg)
            in case i of
                Nothing  -> exec' is $ next : result
                Just val -> exec' is $ (lastCycle + 2, lastReg + val) : next : result

parse :: String -> [Instruction]
parse = map parse' . lines
    where
        parse' :: String -> Instruction
        parse' "noop" = Nothing
        parse' txt    = Just $ read $ drop 5 txt
