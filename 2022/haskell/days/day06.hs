import AoC ( getInput, getExample )

import qualified Data.Set as S

main :: IO ()
main = do
    i <- readFile $ getInput 6
    e <- readFile $ getExample 6

    print $ "Part1 example: " ++ (show $ findMarker e 4)
    print $ "Part1 input  : " ++ (show $ findMarker i 4)
    
    print $ "Part1 example: " ++ (show $ findMarker e 14)
    print $ "Part1 input  : " ++ (show $ findMarker i 14)

findMarker :: String -> Int -> Int
findMarker = findMarker' 0
    where
        findMarker' n str len = 
            if len == (length $ S.fromList $ (take len . drop n) str)
            then len + n --We need to end-point, not the start of the match
            else findMarker' (n + 1) str len