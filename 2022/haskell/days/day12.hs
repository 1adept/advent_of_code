import AoC
import qualified Data.Set as S
import Data.Char (ord)
import Data.Ord (clamp)
import Data.Maybe (fromJust)
import Data.Foldable (find)

type Map        = [Int]
type MapDim     = (Int, Int)
type MapData    = (Map, MapDim)

myEx, myIn :: IO MapData
myEx = do 
    i <- readFile $ getExample day
    return . parse $ i
myIn = do  
    i <- readFile $ getInput day
    return . parse $ i

day :: Int
day = 12

main :: IO ()
main = do
    myEx >>= (print . ("Part (1, 2) example = " ++) . show) . solve
    myIn >>= (print . ("Part (1, 2) input   = " ++) . show) . solve

    putStrLn $ "Good night day " <> show day

solve :: MapData -> (Int, Int)
solve md = (bfs md, bfsDescending md)

bfs, bfsDescending :: MapData -> Int
bfs           (m, d) = bfsStep (m, d) isEnd    canReach        S.empty ([findPos m isStart], []) 0
bfsDescending (m, d) = bfsStep (m, d) isGround (flip canReach) S.empty ([findPos m isEnd], [])   0

findPos :: Map -> (Int -> Bool) -> Int
findPos m p = fromJust $ find (p . get m) [0 .. (length m - 1)]

bfsStep :: (Map, MapDim)
        -> (Int -> Bool)        -- Is the given Height the target
        -> (Int -> Int -> Bool) -- Can reach from one height to another
        -> S.Set Int            -- Seen Indices
        -> ([Int], [Int])       -- TODO (Indices in current 'numSteps' distance, Indices in next 'numSteps' distance)
        -> Int                  -- Num Steps (for fst todo)
        -> Int                  -- Num Steps
bfsStep _      _        _        _    ([], [])           numSteps = numSteps
bfsStep (m, d) isTarget canClimb seen ([], next)         numSteps = bfsStep (m, d) isTarget canClimb seen (next, []) (numSteps + 1)
bfsStep (m, d) isTarget canClimb seen (current:xs, next) numSteps = 
    let reached = isTarget . get m $ current
        seen'   = S.insert current seen
        height  = get m current
        dirs    = filter (\i -> 
                    (canClimb height . get m $ i)
                    && i `S.notMember` seen)
                $ neighbors (m, d) current
        todo'   = if reached 
                then ([], [])           -- Done
                else (xs, next ++ dirs)
    in if S.member current seen
        then def seen (xs, next) numSteps
        else def seen' todo'     numSteps
    where def = bfsStep (m, d) isTarget canClimb

-- Is the given height the start/end
isStart, isEnd, isGround :: Int -> Bool
isEnd    = (== charToHeight 'E')
isStart  = (== charToHeight 'S')
isGround = (<= charToHeight 'a')

neighbors :: (Map, MapDim) -> Int -> [Int]
neighbors (m, d) current = map fromJust 
                        . filter (/= Nothing) 
                        . map (\f -> f d current) 
                        $ [upIndex, downIndex, leftIndex, rightIndex]

canReach :: Int -> Int -> Bool
canReach from to = (clp from + 1) >= clp to
    where clp = clamp (1, 26)

get :: Map -> Int -> Int
get map index = map !! index

upIndex, downIndex, leftIndex, rightIndex :: MapDim -> Int -> Maybe Int
upIndex (w, _) index 
    | pos >= 0  = Just pos
    | otherwise = Nothing
    where pos = index - w
downIndex (w, h) index 
    | pos < (w * h) = Just pos
    | otherwise      = Nothing
    where pos = index + w
leftIndex (w, _) index 
    | (index `mod` w) > 0 = Just (index - 1)
    | otherwise           = Nothing
rightIndex (w, h) index
    | pos >= 0 && pos < (w - 1) = Just (index + 1)
    | otherwise                 = Nothing
    where pos = index `mod` w

-- Heightmap and the size of the map
parse :: String -> MapData
parse txt = (concatMap pLine lns, mapDim)
    where
        lns = lines txt
        mapDim = (length . head $ lns, length lns)
        pLine  = map charToHeight

charToHeight :: Char -> Int
charToHeight c 
    | c == 'S'   = height $ ord 'a' - 1
    | c == 'E'   = height $ ord 'z' + 1
    | otherwise  = height $ ord c
    where height = subtract (ord 'a' - 1)