import AoC

import Data.List
import GHC.Read (readField)
import Debug.Trace (trace)
import Text.Printf (printf)

type Instruction = Maybe Int
type State = (Int, Int)         -- (Register value, Cycle, Cycle behind)

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

solve1 :: [Instruction] -> Int
solve1 = sum 
        . map (uncurry (*))
        . filter (\(cycle, _) -> cycle >= 20 && (cycle - 20) `mod` 40 == 0) 
        . exec

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
