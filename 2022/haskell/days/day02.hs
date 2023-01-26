import AoC
import Data.Char ( isSpace ) 

data Shape = Rock | Paper | Scissors
    deriving (Eq, Show, Ord, Enum)
data Result = Loss | Draw | Win
    deriving (Eq, Show, Ord, Enum)
data Round = Round Shape Shape 
    deriving (Eq, Show)

main :: IO ()
main = do 
    content <- readFile $ getInput 2

    let rounds = map (parseRound) $ lines content

    let part1 = sum $ map scoreRound rounds
    print $ "Part1 " ++ show part1

    let part2 = sum $ map (scoreRound . amendRound) rounds
    print $ "Part2 " ++ show part2

amendRound :: Round -> Round
amendRound (Round p1 p2) = Round p1 (playResult result p1)
    where 
        result = case p2 of
            Rock      -> Loss
            Paper     -> Draw
            _         -> Win

scoreShape :: Shape -> Int
scoreShape sh = 1 + fromEnum sh

scoreRound :: Round -> Int
scoreRound r@(Round _ sh) = 3 * (fromEnum $ judgePlayer2 r) + scoreShape sh

judgePlayer2 :: Round -> Result
judgePlayer2 (Round x y) 
    | [Rock, Paper]     == sit = Win
    | [Paper, Scissors] == sit = Win
    | [Scissors, Rock]  == sit = Win
    | x == y                   = Draw
    | otherwise                = Loss
    where
        sit = [x, y]

parseRound :: String -> Round
parseRound str =
    let (left, right) = splitAt 1 str
        trim = reverse . dropWhile isSpace
        toShape = parseShape . trim
    in Round (toShape left) (toShape right)

parseShape :: String -> Shape
parseShape sh 
    | sh == "X" || sh == "A" = Rock
    | sh == "Y" || sh == "B" = Paper
    | otherwise              = Scissors

playResult :: Result -> Shape -> Shape 
playResult res sh = play strategy sh
    where 
        play :: (Int -> Int) -> Shape -> Shape
        play dir = toEnum . (flip mod 3) . dir . (+3) . fromEnum
        strategy = case res of
            Win -> (+1)
            Draw -> (+0)
            Loss -> (subtract 1)
