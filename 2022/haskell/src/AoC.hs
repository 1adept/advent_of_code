module AoC ( getInputFileName ) where

import System.Environment ( getArgs, getProgName )

getInputFileName :: IO String
getInputFileName = do
    args <- getArgs
    progName <- getProgName
    let baseFileName =  if null args
                        then progName
                        else head args
    let dataFileName = "../input/" ++ baseFileName ++ ".txt"
    return dataFileName