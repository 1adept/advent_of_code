{-# LANGUAGE InstanceSigs #-}
import AoC

import qualified Data.Set        as S
import qualified Data.Map.Strict as M
import Data.List (groupBy, sort)
import Debug.Trace (trace)
import Text.Printf (printf)

data MonkeyRule = MonkeyRule
    { id        :: Int
    , operation :: Int -> Int
    , test      :: Int -> Bool
    , onTrue    :: Int
    , onFalse   :: Int
    }
    
instance Eq MonkeyRule where
    (==) :: MonkeyRule -> MonkeyRule -> Bool
    (==) lhs rhs = Main.id lhs == Main.id rhs
instance Ord MonkeyRule where
    compare :: MonkeyRule -> MonkeyRule -> Ordering
    compare lhs rhs = compare (Main.id lhs) (Main.id rhs)
type Monkey = ([Int], MonkeyRule)

day :: Int
day = 11

myEx :: IO (M.Map Int Monkey)
myEx = do
    ex <- readFile $ getExample day
    return $ monkeyMap $ parse ex
myIn :: IO (M.Map Int Monkey)
myIn = do
    inp <- readFile $ getInput day
    return $ monkeyMap $ parse inp

main :: IO ()
main = do
    mme <- myEx
    mmi <- myIn

    let solve1 m = exec m (20, (`div` 3))
    let solve2 m = exec m (10000, (`mod` (commonMonkeyDivider $ M.elems m)))

    putStrLn $ "Part1 example: " <> show (solve1 mme)
    putStrLn $ "Part2 example: " <> show (solve2 mme)

    putStrLn $ "Part1 input: " <> show (solve1 mmi)
    putStrLn $ "Part2 input: " <> show (solve2 mmi)

    putStrLn $ "Good night day " ++ show day

exec :: M.Map Int Monkey -> (Int, Int -> Int) -> Int
exec monkeys (rounds, relax) = 
    let init = M.fromList $ zip (M.keys monkeys) [0,0..]
        (rM, rI) = doRoundsCountingInspects rounds relax (monkeys, init)
        all = reverse . sort . M.elems $ rI
    in product $ take 2 all
    where
        doRoundsCountingInspects :: Int -> (Int -> Int) -> (M.Map Int Monkey, M.Map Int Int) -> (M.Map Int Monkey, M.Map Int Int)
        doRoundsCountingInspects 0 _ result = result
        doRoundsCountingInspects r relax result = doRoundsCountingInspects (r - 1) relax $ round relax result

        round :: (Int -> Int) -> (M.Map Int Monkey, M.Map Int Int) -> (M.Map Int Monkey, M.Map Int Int)
        round relax (monkeys, inspects) = foldl turn (monkeys, inspects) $ sort $ M.keys monkeys
            where 
                turn :: (M.Map Int Monkey, M.Map Int Int) -> Int -> (M.Map Int Monkey, M.Map Int Int)
                turn (monkeys, inspects) monkeyId = 
                    let (nextMonkeys, numInspects) = monkeyTurn relax monkeys monkeyId
                        nextInspects = M.insert monkeyId (numInspects + inspects M.! monkeyId) inspects
                    in (nextMonkeys, nextInspects)

        monkeyTurn :: (Int -> Int) -> M.Map Int Monkey -> Int -> (M.Map Int Monkey, Int)
        monkeyTurn relax monkeyMap n = 
            let monkey@(items, rule) = monkeyMap M.! n
                (inspectedTrue, inspectedFalse) = inspect relax monkey
                map1 = M.insert n ([], rule) monkeyMap
                toTrue = onTrue rule
                (trueItems, trueRule) = monkeyMap M.! toTrue
                map2 = M.insert toTrue (trueItems ++ inspectedTrue, trueRule) map1
                toFalse = onFalse rule
                (falseItems, falseRule) = monkeyMap M.! toFalse
                map3 = M.insert toFalse (falseItems ++ inspectedFalse, falseRule) map2
            in (map3, length inspectedTrue + length inspectedFalse)

        inspect :: (Int -> Int) -> Monkey -> ([Int], [Int])
        inspect relax (items, rule) =
            let op = operation rule
                tst = test rule
                (l, r) = foldl  
                    (flip (handleItem relax op tst))
                    ([], [])
                    items
            in (reverse l, reverse r)
            where
                handleItem :: (Int -> Int) -> (Int -> Int) -> (Int -> Bool) -> Int -> ([Int], [Int]) -> ([Int], [Int])
                handleItem relax op test i (l, r) =
                    let w = relax $ op i
                    in if test w 
                            then (w : l, r)
                            else (l, w : r)

countInspects :: M.Map Int Monkey -> Int
countInspects = sum . map (length . fst) . M.elems

monkeyMap :: [Monkey] -> M.Map Int Monkey
monkeyMap monkeys = M.fromList $ map (\m -> (Main.id . snd $ m ,m)) monkeys

commonMonkeyDivider :: [Monkey] -> Int
commonMonkeyDivider = product . map (findVal 1 . test . snd)
    where
        findVal :: Int -> (Int -> Bool) -> Int
        findVal val op = if op val
                then val
                else findVal (val + 1) op

parse :: String -> [Monkey]
parse txt = parse' (lines txt) []
    where
        parse' :: [String] -> [Monkey] -> [Monkey]
        parse' [] monkeys  = reverse monkeys
        parse' txt monkeys =
            let monkey = parse1 $ take 6 txt
                todo   = drop 7 txt -- drop newline too
            in parse' todo (monkey : monkeys) 

        parse1 :: [String] -> ([Int], MonkeyRule)
        parse1 datas = 
            let (idStr:is:op:tst:true:false) = datas
                id          = parseId idStr
                operation   = parseOp op
                test        = parseTest tst
                onTrue      = parseThrow true
                onFalse     = parseThrow $ head false
            in (parseItems is, MonkeyRule id operation test onTrue onFalse)

        parseId :: String -> Int
        parseId = read . takeWhile (/= ':') . last . words

        parseItems :: String -> [Int]
        parseItems = numbers . drop 1 . dropWhile (/= ':')

        parseOp :: String -> (Int -> Int)
        parseOp line = 
            let [factor, op] = take 2 . reverse . words $ line -- e.g ["old", "+"]
                operation    = if op == "+" then (+) else (*)
                toSelf       = factor == "old"
            in \x -> operation x $ if toSelf then x else read factor

        parseTest :: String -> (Int -> Bool)
        parseTest line = 
            let factor = read . last . words $ line
            in \x -> 0 == x `mod` factor

        parseThrow :: String -> Int
        parseThrow = read . last . words

        numbers :: String -> [Int]
        numbers = map (read . takeWhile (/= ',')) . words      