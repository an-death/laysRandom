{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lays
import Control.Monad (forM_, foldM)
import HTTP (newSession)
import GHC.IO.Encoding

jwt_s = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzZXNzaW9uIjoiN2YwZmFhYTItNDM3Zi00NWZkLWE0OTQtYTBiNzVhM2I5YzcyIiwiaWF0IjoxNTY3NDk4NjU1fQ.UQj1Fxt65nElzlWtM6fe5TvfyrbSYbmujN4BL5BHs0k"

main :: IO ()
main = do
    setLocaleEncoding utf8
    goodCodes <- readGoodCodes
    session <- newSession jwt_s
    forM_ goodCodes $ \goodCode -> do
        putStrLn $ "Get new goodCode " ++ goodCode
        -- after 10 tryes you'll be banned
        pushRandomCodes session 8
        -- try send good code to refresh ban counter
        putStrLn $ "Random tryes left. Tryed good code `" ++ goodCode ++ "`"
        resp <- pushLaysCode session goodCode
        case resp of 
            (Right ansv) -> putStrLn "GOOD! Refresh counter" >> return ()
            (Left ansv)  -> error $ "Oooops! GoodCode failed! " ++ ansv
    
    putStrLn "No codes left"



