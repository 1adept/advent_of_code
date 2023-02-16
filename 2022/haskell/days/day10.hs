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

    print $ show $ exe e

    putStrLn $ "Hello day " ++ show day


totalCycles :: [Instruction] -> Int
totalCycles ins = sum $ fmap toCycle ins
    where
        toCycle :: Instruction -> Int
        toCycle Nothing = 1
        toCycle _       = 2

exe :: [Instruction] -> Int
exe instructions = 
    let (total, cycle, reg) = exe' instructions False (0, 1, 1)
    in total
    where
        exe' :: [Instruction] -> Bool -> (Int, Int, Int) -> (Int, Int, Int)
        exe' []         _    result                = result
        exe' ins@(i:is) idle (total, cycle, reg)   = 
            let newTotal = if record cycle
                    then trace 
                        (printf "Cycle %3d with register %d => strength %4d, total = %5d" 
                            cycle reg (cycle * reg) (total + cycle * reg)) 
                        (total + cycle * reg)
                    else total
            in case i of
                Nothing -> exe' is False (newTotal, cycle + 1, reg)
                Just val -> if idle
                    then exe' is False (newTotal, cycle + 1, reg + val)
                    else exe' ins True (newTotal, cycle + 1, reg)

        record :: Int -> Bool
        record cycle = cycle >= 20 && (cycle - 20) `mod` 40 == 0

parse :: String -> [Instruction]
parse = map parse' . lines
    where
        parse' :: String -> Instruction
        parse' "noop" = Nothing
        parse' txt    = Just $ read $ drop 5 txt
