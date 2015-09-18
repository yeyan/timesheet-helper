import Numeric 
import Parser
import System.Environment

import Control.Monad
import Control.Applicative

import Data.Ord
import Data.Time
import Data.List

--intervals records = 
--    foldl merge [] $ zipWith (\(_, t1) (label, t2) -> (label, t2 - t1)) records $ tail records
--
--merge rs (label, time) = 
--    filter (\(l, _) -> l /= label) rs ++ [(label, time + maybe 0 id (lookup label rs))]
--
--formatFloatN floatNum numOfDecimals = 
--    showFFloat (Just numOfDecimals) floatNum ""
--
--printIntervals file []      = putStrLn "Not enough time record"
--printIntervals file records = do
--    let maxLabelLen = foldl1 max $ map (length . fst) records
--        maxLineLen = maxLabelLen + 12
--        printHorizontalLine = putStrLn $ replicate maxLineLen '='
--    putStrLn file
--    printHorizontalLine
--    mapM_ (\(label, time) -> putStrLn $ "| " ++ label ++ replicate (maxLabelLen - length label) ' ' ++ " | " ++ formatFloatN time 2 ++ "h |") records
--    printHorizontalLine
--
--main = do
--    args <- getArgs
--    forM_ args $ \file -> do
--        result <- liftA intervals <$> parseTimeSheet file
--        case result of
--            Left error -> putStrLn $ "Error: " ++ show error
--            Right records -> printIntervals file records
--

--merge rs (label, time) = 
--    filter (\(l, _) -> l /= label) rs ++ [(label, time + maybe 0 id (lookup label rs))]

intervals entries = 
    sortBy (\(n1, _) (n2, _) -> compare n1 n2) $ foldl merge [] $ 
        zipWith (\(t1, _) (t2, name) -> (name, t2 - t1)) records $ tail records
    where
        records = 
            map (\(time, name) -> (timeOfDayToTime (localTimeOfDay time), name)) entries
        merge rs (label, time) = 
            filter (\(l, _) -> l /= label) rs ++ [(label, time + maybe (secondsToDiffTime 0) id (lookup label rs))]

printTable rows = do
    let colWidths  = map (foldl1 max) $ transpose $ map (map length) rows
        tableWidth = length colWidths * 3 + 1 + sum colWidths
        cell (len, msg) = " " ++ msg ++ " " ++ replicate (len - length msg) ' ' 
        seperator = putStrLn $ replicate tableWidth '='
    seperator
    mapM_ putStrLn $ map ( (++ "|") . ((++) "|") . concat . intersperse "|") $ map (map cell) $ map (zip colWidths) rows 
    seperator

tableColumnSizes :: [[String]] -> [Int]
tableColumnSizes = map (foldl1 max) . transpose . map (map length)

alignL (size, text) = replicate (size - length text) ' ' ++ text ++ " "
alignR (size, text) = " " ++ text ++ replicate (size - length text) ' '
alignC (size, text) = 
    let p = size - length text
        l = floor $ toRational p / 2
        r = p - l
    in replicate l ' ' ++ text ++ replicate r ' '

tableRow = map ((++ "|") . ((++) "|") . concat . intersperse "|")
tableSeperator size = replicate size '='

printT table = do
    let colWidths  = map (foldl1 max) $ transpose $ map (map length) $ concat $ map snd table
        tableWidth = length colWidths * 3 + 1 + sum colWidths
        seperator  = putStrLn $ tableSeperator tableWidth
        cell (len, msg) = " " ++ msg ++ " " ++ replicate (len - length msg) ' ' 
    seperator
    forM_ table $ \(day, intervals) -> do
        putStrLn $ (++ "|") $ ((++) "|") $alignC (tableWidth - 2, (show day))
        mapM_ putStrLn $ map ( (++ "|") . ((++) "|") . concat . intersperse "|") $ map (map cell) $ map (zip colWidths) intervals
        seperator

main = do
    args <- getArgs
    forM_ args $ \file -> do
        result <- parseTimeSheet file
        case result of
            Left error -> putStrLn $ "Error: " ++ show error
            Right entries -> do --printIntervals file records
                let showInHour secs = showFFloat (Just 2) (secs / 3600) "h"
                    day = localDay . fst
                    present = map $ \(name, time) -> [name, showInHour (fromRational (toRational time))]
                    process = map (\group -> ((day . head) group, (present . intervals) group)) . groupBy (\e1 e2 -> day e1 == day e2)
                    results = process entries
                printT results

test = do
    result <- parseTimeSheet "test.txt"
    let Right entries = result
        showInHour secs = showFFloat (Just 2) (secs / 3600) "h"

        day = localDay . fst
        present = map $ \(name, time) -> [name, showInHour (fromRational (toRational time))]
        process = map (\group -> ((day . head) group, (present . intervals) group)) . groupBy (\e1 e2 -> day e1 == day e2)
        results = process entries
    printT results
    --putStrLn $ show $ groupBy (\e1 e2 -> localDay (fst e1) == localDay (fst e2)) entries
    --return $ map intervals $ groupBy (\e1 e2 -> localDay (fst e1) == localDay (fst e2)) entries
    --x <- return $ map (\(name, time) -> [name, showInHour (fromRational (toRational time))]) $ map intervals entries
    --printTable x

