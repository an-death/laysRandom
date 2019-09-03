{-# LANGUAGE OverloadedStrings #-}
module HTTP where

import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Lens
import Network.Wreq
import Data.Aeson.Lens (_String, _Integer, key)
import Time 

import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (unpack)

import qualified Network.HTTP.Client as HTTPClient
import qualified Network.Wreq.Session as S
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Time.Clock as Time


_USER_AGENT = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/75.0.3770.100 Safari/537.36" 
_USER_ID = "4322fa64-1b12-4ba9-8b2b-9d0cc7b84245"

defaultOptions :: Options
defaultOptions = defaults 
    & header "User-Agent" .~ [_USER_AGENT]
    & header "Content-Type" .~ ["application/x-www-form-urlencoded; charset=UTF-8"]
    & header "Accept" .~  ["application/json"]
    & header "Referer" .~ ["https://laysmusic.ru/"]
    & header "X-Requested-With" .~ ["XMLHttpRequest"]

data Session = Session {
    _jwt_session_id :: IORef Strict.ByteString,
    _httpSession :: S.Session
}

newSession :: Strict.ByteString ->  IO Session
newSession prevSession = do 
    ref <- newIORef prevSession
    httpSession <- S.newSession
    return $ Session ref httpSession



pushCode :: Session -> String -> IO (Either String Integer)
pushCode session code = do
    options <- getToken session >>= \t -> newCookieJar _USER_ID (encodeUtf8 t) >>= \jar -> return $ opts jar
    r <- push_code options code

    case (r ^? responseBody . key "task_id". _String) of 
        (Nothing)       -> returnError "No Task Id " r
        (Just task_id)  -> do
            threadDelay (seconds 1)
            r <- push_task options task_id
            case (r ^? responseBody . key "status") of 
                (Just "success")    -> returnPoints r
                _                   ->  returnError "Push code failed! response: " r

    where 
        httpSession = _httpSession session
        getToken ses = requestToken ses >>= \r -> return $ r ^. responseBody . key "jwt" . _String
        opts cookieJar = defaultOptions & cookies .~ (Just cookieJar)
        
        post opts = S.postWith opts httpSession
        push_code opts code = post opts "https://laysmusic.ru/method/check_code/" ["code" := code]
        push_task opts task_id = post opts "https://laysmusic.ru/method/check_task/" ["task_id" := task_id]
        
        returnError msg r = return $ Left (msg ++ ( unpack (r ^. responseBody . key "message" . _String)))
        returnPoints r = return $ Right $ read $ unpack (r ^. responseBody . key "points" . _String)
        

requestToken ses = do 
    last_jwt_ses <- readIORef (_jwt_session_id ses)
    r <- S.getWith (opts last_jwt_ses) httpSession "https://pass.pepsico.digital/api/auth/token"

    case (r ^? responseHeader "X-Jwt-Session") of 
        (Just jwt_ses)   -> updateJWTSessionId jwt_ses >>  return r
        (Nothing)        -> error $ "Cannot request JWT-Session " ++ (show r)
    
    where
        opts jwt_ses = defaults 
            & header "Accept"  .~ ["application/json"]
            & header "Referer" .~ ["https://laysmusic.ru/"]
            & header "Origin"  .~ ["https://laysmusic.ru"]
            & header "User-Agent" .~ [_USER_AGENT]
            & header "x-jwt-session" .~ [jwt_ses]
            & header "X-Project-Id" .~ ["837"]

        ref = _jwt_session_id ses
        httpSession = _httpSession ses
        updateJWTSessionId = writeIORef ref 


newCookieJar user_id jwt = do
    now <- Time.getCurrentTime
    let expires = Time.addUTCTime (days 30) now
        cookieCreate name value =  HTTPClient.Cookie name value expires "laysmusic.ru" "/" now now True True False False
        jar = HTTPClient.createCookieJar [
              cookieCreate "bp_jwt_token" jwt 
            , cookieCreate "bp_user_guid" user_id
            , cookieCreate "_ga" "GA1.2.934041142.1563707695"
            , cookieCreate "_gid" "GA1.2.1809806867.1563707695"
            , cookieCreate "_ym_uid" "1563707695974707088"
            , cookieCreate "_ym_d" "1563707695"
            , cookieCreate "_ym_isad" "1"
            , cookieCreate "_ym_visorc_54045625" "w"
            , cookieCreate "_gat_UA-90926084-3" "1" 
            ]
    return jar
