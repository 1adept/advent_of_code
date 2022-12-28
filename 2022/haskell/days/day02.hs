import AoC
import Data.Attoparsec.Text hiding (Result)

data Shape = Rock | Paper | Scissors
    deriving (Eq, Show, Ord, Enum)
data Result = Draw | Win | Loss
    deriving (Eq, Show, Ord, Enum)
data Round = Round Shape Shape 
    deriving (Eq, Show)

main :: IO ()
main = do 
    input <- getInputFileName
    content <- readFile input

    -- putStrLn content

    let rounds = [parseRound r | r <- lines content]
    putStrLn . show $ rounds

    -- putStrLn . show $ content
    putStrLn "NOPER"

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
parseRound str = Round p1 p2
    where 
        [p1, p2] = map parseShape $ sepBy str " "
    

parseShape :: String -> Shape
parseShape sh 
    | sh == "X" || sh == "A" = Rock
    | sh == "Y" || sh == "B" = Paper
    | otherwise              = Scissors