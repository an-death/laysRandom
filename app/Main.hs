{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lays
import Control.Monad (forM_, foldM)
import HTTP (newSession)
import GHC.IO.Encoding
import System.Environment (getArgs)
import Control.Monad (when)
import Data.ByteString.Char8 (pack)

main :: IO ()
main = do
    setLocaleEncoding utf8
    [jwt_s] <- getArgs
    when (length jwt_s < 171) (error "zero length jwt token")
    goodCodes <- readGoodCodes
    session <- newSession (pack jwt_s)
    forM_ goodCodes $ \goodCode -> do
        putStrLn $ "Get new goodCode " ++ goodCode
--         after 10 tryes you'll be banned
        pushRandomCodes session 8
--         try send good code to refresh ban counter
        putStrLn $ "Random tryes left. Tryed good code `" ++ goodCode ++ "`"
        resp <- pushLaysCode session goodCode
        case resp of
            (Right ansv) -> putStrLn "GOOD! Refresh counter" >> return ()
            (Left ansv)  -> error $ "Oooops! GoodCode failed! " ++ ansv
    
    putStrLn "No codes left"



