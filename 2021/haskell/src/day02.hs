import AoC

data Dir
    = Forward
    | Down
    | Up
    deriving (Show, Eq)
data Move = Move Dir Int
    deriving (Show, Eq)
type Coord = (Int, Int, Int)

main :: IO ()
main = do 
    example <- readFile $ getExample 2
    input <- readFile $ getInput 2

    let movesEx = parse example
    let movesIn = parse input

    let finalPos = followInstructionsBy (0,0,0) applyMove
    let finalPosAim = followInstructionsBy (0,0,0) applyAim

    print $ "Part1 example: " ++ show (mulTuple (finalPos movesEx))
    print $ "Part1 input: " ++ show (mulTuple (finalPos movesIn))
    
    print $ "Part2 example: " ++ show (mulTuple (finalPosAim movesEx))
    print $ "Part2 input: " ++ show (mulTuple (finalPosAim movesIn))

mulTuple :: Coord -> Int
mulTuple (hor, depth, _aim) = hor * depth

followInstructionsBy :: Coord -> (Coord -> Move -> Coord) -> [Move] -> Coord
followInstructionsBy pos _ [] = pos
followInstructionsBy pos how moves =
    let (first:rest) = moves
        next = how pos first
    in followInstructionsBy next how rest

applyMove, applyAim :: Coord -> Move -> Coord
applyMove (hor, depth, aim) (Move dir num) = case dir of
    Forward -> (hor + num, depth, aim)
    Up      -> (hor, depth - num, aim - num)
    Down    -> (hor, depth + num, aim + num)
applyAim (hor, depth, aim) (Move dir num) = case dir of 
    Forward -> (hor + num, depth + (aim * num), aim      )
    Up      -> (hor,       depth,               aim - num)
    Down    -> (hor,       depth,               aim + num)

parse :: String -> [Move]
parse = map parseLine <$> lines

parseLine :: String -> Move
parseLine line = 
    let [dir, num] = words line
        toDir :: String -> Dir
        toDir "down" = Down
        toDir "up"   = Up
        toDir _      = Forward
    in Move (toDir dir) (read num)