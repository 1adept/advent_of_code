module AoC (getExample, getInput) where

import Text.Printf ( printf )

getInput, getExample :: Int -> String
getInput = getFilePath "input"
getExample = getFilePath "example"

getFilePath :: String -> Int -> String
getFilePath str day = "../" ++ str ++ "/" ++ getFileName day

getFileName :: Int -> String
getFileName = flip (++) (".in") . printf "day%02d"