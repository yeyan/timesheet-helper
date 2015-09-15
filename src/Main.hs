import Numeric 
import Parser
import System.Environment

import Control.Monad
import Control.Applicative

intervals records =
    foldl merge [] $ zipWith (\(_, t1) (label, t2) -> (label, t2 - t1)) records $ tail records

merge rs (label, time) = 
    filter (\(l, _) -> l /= label) rs ++ [(label, time + maybe 0 id (lookup label rs))]

formatFloatN floatNum numOfDecimals = 
    showFFloat (Just numOfDecimals) floatNum ""

main = do
    args <- getArgs
    forM_ args $ \file -> do
        result <- liftA intervals <$> parseTimeSheet file
        putStrLn $ show result
        case result of
            Left error -> putStrLn $ "Error: " ++ show error
            Right records -> do
                let maxLabelLen = foldl1 max $ map (length . fst) records
                    maxLineLen = maxLabelLen + 12
                    printHorizontalLine = putStrLn $ replicate maxLineLen '='
                putStrLn $ "* " ++ file
                printHorizontalLine
                mapM_ (\(label, time) -> putStrLn $ "| " ++ label ++ replicate (maxLabelLen - length label) ' ' ++ " | " ++ formatFloatN time 2 ++ "h |") records
                printHorizontalLine

