{-# LANGUAGE OverloadedStrings #-}

import AoC
import Data.List

data Dir
    = Dir [Dir]
    | File Int
    deriving (Show)

day :: Int
day = 7

main :: IO ()
main = do
    e <- readFile $ getExample day
    i <- readFile $ getInput day

    print "Day: 7"
    let root = snd . flip getFs (Dir []) . drop 2 . lines
    let dirSizes = getDirSizes . root

    let eRoot = root e
    let iRoot = root i

    print $ "Part1 example: " ++ (show $ sumLessThan10k $ getDirSizes eRoot)
    print $ "Part1 input  : " ++ (show $ sumLessThan10k $ getDirSizes iRoot)

    print $ "Part2 example: " ++ (show $ smallestToDelete eRoot)
    print $ "Part2 input  : " ++ (show $ smallestToDelete iRoot)
    where
        sumLessThan10k :: [Int] -> Int
        sumLessThan10k = sum . filter (< 100000)
        smallestToDelete :: Dir -> Int
        smallestToDelete root =
                minimum 
                . filter (>= (30000000 - unused root))
                $ getDirSizes root
        unused :: Dir -> Int
        unused = (flip subtract 70000000) . getSize

getDirSizes :: Dir -> [Int]
getDirSizes (File _) = []
getDirSizes dir@(Dir files) = 
    getSize dir : foldl (\a d -> a ++ getDirSizes d) [] files

getSize :: Dir -> Int
getSize (File size) = size
getSize (Dir files) = sum . map getSize $ files

getFs :: [String] -> Dir -> ([String], Dir)
getFs [] fs = ([], fs)
getFs (cmd : cmds) dir@(Dir files) 
    | cmd == "$ cd .."        = (cmds, dir)
    | "dir" `isPrefixOf` cmd  = getFs cmds dir
    | "$ ls" `isPrefixOf` cmd = getFs cmds dir
    | "$ cd" `isPrefixOf` cmd = getFs nCmd (Dir (sfs : files))
    | otherwise               = getFs cmds (Dir (nf : files))
    where
        [size, _]     = words cmd
        nf            = File (read size)
        (nCmd, sfs)   = getFs cmds (Dir [])