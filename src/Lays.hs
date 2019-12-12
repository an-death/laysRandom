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
--	"Clepjlts",
	"Sumety2a",
	"Sklfmxsk",
	"Sky5vyde",
	"Cl7mx25x",
	"Cl4m7mas",
	"Clsr5x5w",
	"Silnzwac",
	"Sk4l7ry7",
	"CLTZLH35",
	"Claumnjc",
	"Clxrusdq",
	"Cldx4klp",
	"Skrc29fp",
	"Clsq23xz",
	"Clyftatw",
	"Clm7m47l",
	"C0uvvvj3",
	"Cl9kc2tr",
	"Clvc97nj",
	"Clgvzyby",
	"Cl4eay9k",
	"Skyy7qcl",
	"С0es7ctl",
	"Clycmhjj",
	"Cl5vt9en",
	"C0c29kc3",
	"Clpy2yjm",
	"Cl5xwpfa",
	"Surakdh4",
	"Spcrcly4n",
	"Spt9fltxy",
	"Srn7ss5l"
    ]
