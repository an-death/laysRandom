module Lays where

import Random
import Control.Concurrent (threadDelay)
import HTTP (newSession, pushCode, Session)

source = ['0'..'9'] ++ ['a' .. 'z']
codeSize = 6 -- 8

getLaysRandomCode = getRandomElementsFromRange codeSize source
seconds = 1000000 

pushLaysCode :: Session -> String -> IO (Either String Integer)
pushLaysCode ses code = do 
    threadDelay (1*seconds)
    pushCode ses code
    -- selectRandomElement [Right "OK", Left "testErr"]

pushRandomCodes :: Session -> Int -> IO ()
pushRandomCodes ses globalTryes = pushLaysCodes' globalTryes "" prefixes
    where
        prefixes = ["cl", "sc", "su", "sk", "sc", "sp", "co"]

        pushLaysCodes' :: Int -> String -> [String] -> IO ()
        pushLaysCodes' 0 _ _ = return ()
        pushLaysCodes' n _ [] = pushLaysCodes' n "" prefixes
        pushLaysCodes' n "" p = getLaysRandomCode >>= \code -> pushLaysCodes' n code p
        pushLaysCodes' tryes code (prefix:px) = do
            resp <- pushLaysCode ses (prefix ++ code)
            case resp of
                (Right _) -> pushRandomCodes ses globalTryes
                (Left err) -> putStrLn err >> pushLaysCodes' (tryes -1) code px


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
