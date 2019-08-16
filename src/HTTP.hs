{-# LANGUAGE OverloadedStrings #-}
module HTTP where

import Data.IORef
import Control.Lens
import Network.Wreq
import Data.Aeson.Lens (_String, key)

import qualified Network.Wreq.Session as S
import qualified Data.ByteString as Strict

_USER_AGENT = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/75.0.3770.100 Safari/537.36" 

push_code sess opts code = S.postWith sess opts "https://laysmusic.ru/method/check_code/" ["code" := code]

push_task sess opts task_id = S.postWith sess opts "https://laysmusic.ru/method/check_task/" ["task_id" := task_id]

opts :: Strict.ByteString -> Strict.ByteString -> Options
opts user_id jwt = defaults 
    & cookie "bp_jwt_token" .~ [jwt] 
    & cookie "bp_user_guid" .~ [user_id]
    & cookie "_ga" .~ ["GA1.2.934041142.1563707695"]
    & cookie "_gid" .~ ["GA1.2.1809806867.1563707695"]
    & cookie "_ym_uid" .~ ["1563707695974707088"]
    & cookie "_ym_d" .~ ["1563707695"]
    & cookie "_ym_isad" .~ ["1"]
    & cookie "_ym_visorc_54045625" .~ ["w"] 
    & cookie "_gat_UA-90926084-3" .~ ["1"]
    & header "User-Agent" .~ [_USER_AGENT]
    & header "Content-Type" .~ ["application/x-www-form-urlencoded; charset=UTF-8"]
    & header "Accept" .~  ["application/json"]
    & header "Referer" .~ ["https://laysmusic.ru/"]
    & header "X-Requested-With" .~ ["XMLHttpRequest"]


token ref sess = do 
    last_jwt_ses <- readIORef ref

    r <- S.getWith (opts last_jwt_ses) sess "https://pass.pepsico.digital/api/auth/token"

    case (r ^? responseHeader "X-Jwt-Session") of 
        (Just jwt_ses) -> writeIORef ref jwt_ses
        (Nothing)        -> error $ "Cannot request JWT-Session " ++ (show r)
    
    return $ r ^. responseBody . key "jwt" . _String
    where
        opts jwt_ses = defaults 
            & header "Accept"  .~ ["application/json"]
            & header "Referer" .~ ["https://laysmusic.ru/"]
            & header "Origin"  .~ ["https://laysmusic.ru"]
            & header "User-Agent" .~ [_USER_AGENT]
            & header "x-jwt-session" .~ [jwt_ses]
            & header "X-Project-Id" .~ ["837"]
