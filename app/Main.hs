module Main where

import Lays
import Control.Monad (forM_, foldM)

jwt = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ0b2tlbiI6IjllZjk5ZTc1LTFkYWEtNGZlNi1iYWJhLTAxNDQ1NzllODFmZSIsImlhdCI6MTU2NTk1OTg5NH0.XGTu2gd0H4KKnmg5oyZCf_YhX5an9bnGWwPCF09VNWo"

main :: IO ()
main = do
    goodCodes <- readGoodCodes
    forM_ goodCodes $ \goodCode -> do
        putStrLn $ "Get new goodCode " ++ goodCode
        -- after 10 tryes you'll be banned
        pushLaysCodes 8
        -- try send good code to refresh ban counter
        putStrLn $ "Random tryes left. Tryed good code `" ++ goodCode ++ "`"
        resp <- pushLaysCode goodCode
        case resp of 
            (Right ansv) -> putStrLn "GOOD! Refresh counter" >> return ()
            (Left ansv)  -> error $ "Oooops! GoodCode failed! " ++ ansv
    
    putStrLn "No codes left"



