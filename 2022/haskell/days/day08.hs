import AoC
import Data.Char
import qualified Data.Set as S

day :: Int
day = 8

type Tree   = Int                       -- Tree / Or the height of one
type Forest = [[Tree]]                  -- Grid of Trees
type Loc2   = (Int, Int)                -- Location 
data Dir    = U | D | L | R             -- Direction
  deriving (Show)
type Vision = (Int, Int, Int, Int)      -- How far can you see from a point

main :: IO ()
main = do
  e <- readFile $ getExample day
  i <- readFile $ getInput   day

  let example = parse e
  let input   = parse i 

  print $ "Part1 example: " ++ (show . countVisible $ example)
  print $ "Part1 input  : " ++ (show . countVisible $ input)

  print $ "Part2 example: " ++ (show $ findMaxScenic example)
  print $ "Part2 input  : " ++ (show $ findMaxScenic input)

dir4 :: [Dir]
dir4 = [U, D, L, R]

findMaxScenic :: Forest -> Int
findMaxScenic forest 
  = maximum
  $ [
    scenicScore forest (x, y)
    | x <- [1..length forest - 2]
    , y <- [1..length forest - 2]
  ]

scenicScore :: Forest -> Loc2 -> Int
scenicScore forest loc = scoreU * scoreD * scoreL * scoreR
  where
    see = visionRange forest  loc
    scoreU = unify $ see U
    scoreD = unify $ see D
    scoreL = unify $ see L
    scoreR = unify $ see R
    unify :: (Int, Int) -> Int
    unify = uncurry (+)

countVisible :: Forest -> Int
countVisible forest 
  = length
  $ filter id
  [
    isVisible forest (x, y)
    | x <- [0..length forest - 1]
    , y <- [0..length forest - 1]
  ]

isVisible :: Forest -> Loc2 -> Bool
isVisible forest loc@(x, y)
  | isEdge forest loc          = True
  |       x     == fst (see U) = True
  | end - x - 1 == fst (see D) = True
  |       y     == fst (see L) = True
  | end - y - 1 == fst (see R) = True
  | otherwise                  = False
  where 
    end = length forest
    see = visionRange forest loc

visionRange :: Forest -> Loc2 -> Dir -> (Int, Int)
visionRange forest start dir = 
  let 
      toEdge      = goEdge forest start dir
      test        = takeWhile (hasVision forest start) toEdge
      len         = length test
      stopper     = if len == length toEdge 
                      then Nothing 
                      else Just (toEdge !! max 0 (length test))
      stopHeight  = get forest <$> stopper
      startLower = (<=) (get forest start) <$> stopHeight
  in case startLower of
    Just True -> (len, 1) -- Trees seen include first higher
    _         -> (len, 0) -- Range until (excluding) tree that is at least same height
        
goEdge :: Forest -> Loc2 -> Dir -> [Loc2]
goEdge forest pos dir = goX' forest (pos `go` dir) dir []
  where
    goX' :: Forest -> Loc2 -> Dir -> [Loc2] -> [Loc2]
    goX' forest pos dir acc 
      | isEdge forest pos = reverse (pos : acc)
      | otherwise         = goX' forest (pos `go` dir) dir (pos : acc)

hasVision :: Forest -> Loc2 -> Loc2 -> Bool
hasVision forest to from = 
  let height = get forest
  in height from < height to

-- Can you see the tree from dir?
canSeeFrom :: Forest -> Loc2 -> Dir -> Bool
canSeeFrom forest loc dir =
  let next  = loc `go` dir
      h1    = get forest loc
      h2    = get forest next
  in h1 > h2

go :: Loc2 -> Dir -> Loc2
go (x, y) dir =
  case dir of
    U -> (x - 1, y)
    D -> (x + 1, y)
    L -> (x, y - 1)
    R -> (x, y + 1)

get :: Forest -> (Int, Int) -> Int
get forest (x, y) = (forest !! x) !! y

-- Lets assume width == height for forest
isEdge :: Forest -> Loc2 -> Bool
isEdge forest (x, y)
  =  x == 0
  || y == 0
  || x == length forest - 1
  || y == length forest - 1

parse :: String -> Forest
parse txt = map parseLine $ lines txt
  where
    parseLine :: String -> [Tree]
    parseLine = map (\c -> read [c])

