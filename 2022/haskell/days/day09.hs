import AoC
import Data.Char 
import Data.Ord (clamp)
import qualified Data.Set as S

import Debug.Trace (trace)
import Text.Printf (printf)
import GHC.Real (reduce)

day :: Int
day = 9

data Dir = U | D | L | R deriving (Eq, Show)
data Ins = Ins Dir Int deriving (Show)

newtype Pos  = Pos (Int, Int)
    deriving (Eq, Show, Ord)
type Knot = Pos
type Rope = [Knot]

main :: IO ()
main = do
    example <- readFile $ getExample day
    let e = parse example
    input <- readFile $ getInput day
    let i = parse input

    let e2 = parse "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"

    let start = S.fromList [Pos (0, 0)]

    let ex1 = play e (rope 2, start)
    let in1 = play i (rope 2, start)
    let ex2 = play e2(rope 10, start)
    let in2 = play i (rope 10, start)
    
    let solve = show . length . snd
    print $ "Part1 example: " ++ solve ex1
    print $ "Part1 input  : " ++ solve in1
    print $ "Part2 example: " ++ solve ex2
    print $ "Part2 input  : " ++ solve in2

play :: [Ins] -> (Rope, S.Set Knot) -> (Rope, S.Set Knot)
play [] score = score
play (i:is) state = play is $ knots state i

knots :: (Rope, S.Set Knot) -> Ins -> (Rope, S.Set Knot)
knots (rope@(head:tail), set) ins = 
    let res@(r, s) = pull ([movePos head ins], set) tail
    in {- trace (printf "Done ins {%s} => \n\t%s\n\t%s" (show "ins") (show r) (show "s")) -} res

pull :: (Rope, S.Set Knot) -> Rope -> (Rope, S.Set Knot)
pull (done, set)          []               = (reverse done, set)
pull (done@(head:_), set) todo@(next:rest) = 
    let (nextKnot, steps) = pullKnot head next
        nextSet           = 
            if null rest 
            then S.union set steps  -- Add steps to result
            else set
    in pull (nextKnot : done, nextSet) rest

-- Knot knot towards target, so that {start} has distance 1 from {target}
pullKnot :: Knot -> Knot -> (Knot, S.Set Knot)
pullKnot target start = stepCollect target (start, S.empty)
    where 
        step :: Pos -> Pos -> Pos
        step p1@(Pos (x1, y1)) p2@(Pos (x2, y2)) = 
            let dx = clamp (-1, 1) (x1 - x2)
                dy = clamp (-1, 1) (y1 - y2)
            in Pos (x2 + dx, y2 + dy) 
        stepCollect :: Knot -> (Knot, S.Set Knot) -> (Knot, S.Set Knot)
        stepCollect target res@(start, set)
            | inRange target start  = res
            | otherwise             = 
                let go = step target start
                    ns = S.insert go set
                in stepCollect target (go, ns)

movePos :: Pos -> Ins -> Pos
movePos (Pos (x, y)) (Ins dir len)
    = case dir of
    U -> Pos (x - len, y)
    D -> Pos (x + len, y) 
    L -> Pos (x, y - len)
    R -> Pos (x, y + len)

add :: Pos -> Pos -> Pos
add (Pos (x, y)) (Pos (a, b)) = Pos (x+a, y+b)

direction :: Pos -> Pos -> Pos
direction (Pos (a, b)) (Pos (c, d)) = 
    let x = clamp (-1, 1) (c - a)
        y = clamp (-1, 1) (d - b)
    in Pos (x, y)

inRange :: Pos -> Pos -> Bool
inRange (Pos (x1, y1)) (Pos (x2, y2)) = dist x1 x2 <= 1 && dist y1 y2 <= 1
    where dist x y = abs (x - y)

rope :: Int -> Rope
rope len = replicate len (Pos (0, 0))

parse :: String -> [Ins]
parse =
    map pIns . filter (not . null) . lines
    where
        pIns txt = Ins (pDir a) (read b)
            where (a, b) = splitAt 2 txt
        pDir "U " = U
        pDir "D " = D
        pDir "L " = L
        pDir "R " = R
        pDir d    = error $ printf "%s is unreachable" (show d)