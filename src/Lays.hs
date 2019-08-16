module Lays where

import Random
import Control.Concurrent (threadDelay)

source = ['0'..'9'] ++ ['a' .. 'z']

getLaysRandomCode = getRandomElementsFromRange 6 source
oneSecond = 1000000 

pushLaysCode :: String -> IO (Either String String)
pushLaysCode code = do 
    threadDelay oneSecond
    putStrLn code
    selectRandomElement [Right "OK", Left "testErr"]

pushLaysCodes :: Int -> IO ()
pushLaysCodes gTryes = pushLaysCodes' gTryes "" prefixes
    where
        prefixes = ["cl", "sc", "su", "sk", "sc", "sp", "co"]

        pushLaysCodes' :: Int -> String -> [String] -> IO ()
        pushLaysCodes' 0 _ _ = return ()
        pushLaysCodes' n _ [] = pushLaysCodes' n "" prefixes
        pushLaysCodes' n "" p = getLaysRandomCode >>= \code -> pushLaysCodes' n code p
        pushLaysCodes' tryes code (prefix:px) = do
            resp <- pushLaysCode (prefix ++ code)
            case resp of
                (Right _) -> pushLaysCodes gTryes
                (Left err) -> print err >> pushLaysCodes' (tryes -1) code px


readGoodCodes :: IO [String]
readGoodCodes = return [
    "c03afefu",
    "clajum8r",
    "schvswtt",
    "cluxjcz2",
    "sumety2a",
    "sklfmxsk",
    "sky5vyde",
    "cl7mx25x",
    "cl4m7mas",
    "clsr5x5w",
    --"clfn4xh[i1l]",
    "silnzwac",
    "sk4l7ry7",
    "cltzlh35",
    
    "claumnjs",
    "clxrusdq",
    "cldx4klp",
    "skrc29fp"
    ]
