{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( startApp
    , app
    ) where

import Control.Monad.Except
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString as BS
import Data.Char (toLower)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as S
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Config
import Fail2banHelper

type OuterAPI = "api" :> API

type API = "version" :> Get '[JSON] APIResponse
      :<|> "logtarget" :> Get '[JSON] APIResponse
      :<|> "dbfile" :> Get '[JSON] APIResponse
      :<|> "dbpurgeage" :> Get '[JSON] APIResponse
      :<|> "jails" :> Get '[JSON] APIResponse
      :<|> "jails" :> JailsAPI
      :<|> "unban" :> Capture "ip" IPAddr :> Post '[JSON] APIResponse
      :<|> "banned" :> Get '[JSON] APIResponse
      :<|> "reload" :> Post '[JSON] APIResponse

type JailsAPI =
  Capture "jail" T.Text :>
     (Get '[JSON] APIResponse
     :<|> "maxretry" :> Capture "retries" Int :> Post '[JSON] APIResponse
     :<|> "findtime" :> Capture "time" Seconds :> Post '[JSON] APIResponse
     :<|> "bantime" :> Capture "time" Seconds :> Post '[JSON] APIResponse
     :<|> "addignore" :> Capture "ip" IPAddr :> Post '[JSON] APIResponse
     :<|> "delignore" :> Capture "ip" IPAddr :> Post '[JSON] APIResponse
     :<|> "ban" :> Capture "ip" IPAddr :> Post '[JSON] APIResponse
     :<|> "unban" :> Capture "ip" IPAddr :> Post '[JSON] APIResponse)

data APIResponse =
    ARError
    { arError :: Bool
    , arReason :: T.Text
    }
  | AROK
  | ARVersion { arVersion :: T.Text }
  | ARLogTarget { arLogTarget :: T.Text }
  | ARDBFile { arDBFile :: T.Text }
  | ARDBPurgeAge { arDBPurgeAge :: Seconds }
  | ARJails { arJails :: [Jail] }
  | ARGetJail
    { arCurrently_Failed :: Int
    , arTotal_Failed :: Int
    , arFile_List :: [T.Text]
    , arCurrently_Banned :: Int
    , arTotal_Banned :: Int
    , arBanned_IPs :: [IPAddr]
    , arBanned_ASNs :: [T.Text]
    , arBanned_Countries :: [T.Text]
    , arBanned_Rirs :: [T.Text]
    , arIgnored_IPs :: [IPAddr]
    , arFindTime :: Seconds
    , arBanTime :: Seconds
    , arMaxRetry :: Int
    }
  | ARBanned { arBanned :: [M.Map Jail [IPAddr]] }

$(deriveJSON defaultOptions{fieldLabelModifier = drop 2 . map toLower, sumEncoding = UntaggedValue} ''APIResponse)

startApp :: IO ()
startApp = run 5000 app

app :: Application
app = serve api server

api :: Proxy OuterAPI
api = Proxy

connectSocket :: Handler S.Socket
connectSocket = do
    sock <- liftIO (S.socket S.AF_UNIX S.Stream S.defaultProtocol)
    liftIO (S.connect sock (S.SockAddrUnix fail2banSockFile))
    return sock

closeSocket :: S.Socket -> Handler ()
closeSocket sock = do
    liftIO (S.sendAll sock (f2bProtoClose `BS.append` f2bProtoEnd))
    liftIO (S.close sock)

withSocket :: F2BCommand -> Handler F2BResponse
withSocket cmd = do
    sock <- connectSocket
    res <- liftIO (sendCommand sock cmd)
    closeSocket sock
    return res

server :: Server OuterAPI
server = innerServer


innerServer :: Server API
innerServer = version
    :<|> logtarget
    :<|> dbfile
    :<|> dbpurgeage
    :<|> jails
    :<|> jailsServer
    :<|> unban
    :<|> banned
    :<|> reload
  where
    errResp resp =
        case resp of
            RespError _ e -> return (ARError True e)
            e             -> return (ARError True (T.pack $ "Got unexpected response: " ++ show e))

    version :: Handler APIResponse
    version = withSocket CmdVersion >>= \resp ->
        case resp of
            RespVersion v -> return (ARVersion v)
            _             -> errResp resp

    logtarget :: Handler APIResponse
    logtarget = withSocket CmdGetLogTarget >>= \resp ->
        case resp of
            RespLogTarget t -> return (ARLogTarget t)
            _               -> errResp resp

    dbfile :: Handler APIResponse
    dbfile = withSocket CmdGetDBFile >>= \resp ->
        case resp of
            RespDBFile f -> return (ARDBFile f)
            _            -> errResp resp


    dbpurgeage :: Handler APIResponse
    dbpurgeage = withSocket CmdGetDBPurgeAge >>= \resp ->
        case resp of
            RespDBPurgeAge a -> return (ARDBPurgeAge a)
            _                -> errResp resp

    jails :: Handler APIResponse
    jails = withSocket CmdStatus >>= \resp ->
        case resp of
            RespStatus (_, (_, jailList)) -> return (ARJails (T.splitOn ", " jailList))
            _                             -> errResp resp

    unban :: IPAddr -> Handler APIResponse
    unban ip = withSocket (CmdUnban [ip]) >>= \resp ->
        case resp of
            RespUnban -> return AROK
            _         -> errResp resp

    banned :: Handler APIResponse
    banned = withSocket CmdBanned >>= \resp ->
        case resp of
            RespBanned ips -> return (ARBanned ips)
            _              -> errResp resp

    reload :: Handler APIResponse
    reload = withSocket (CmdReload True False False) >>= \resp ->
        case resp of
            RespReload -> return AROK
            _          -> errResp resp

jailsServer :: Server JailsAPI
jailsServer jail =
         getjail
    :<|> maxretry
    :<|> findtime
    :<|> bantime
    :<|> addignore
    :<|> delignore
    :<|> ban
    :<|> unban
  where
    testSuccess resp =
        case resp of
            RespSetSuccess -> return AROK
            RespError _ e  -> return (ARError True e)
            e              -> return (ARError True (T.pack $ "Got unexpected response: " ++ show e))

    getjail :: Handler APIResponse
    getjail = withSocket (CmdStatusJailCymru jail) >>= \cymru ->
        case cymru of
            RespStatusJailCymru ((_, ((_, curFailed), (_, totalFailed), (_, fileList))),
                                 (_, ((_, curBanned), (_, totalBanned), (_, bannedIPs),
                                     (_, bannedASNs), (_, bannedCountries), (_, bannedRIRs)))) -> do
                                         (RespGetIgnoreIP ignoreIP) <- withSocket (CmdGetJailIgnoreIP jail)
                                         (RespGetFindTime findTime) <- withSocket (CmdGetJailFindTime jail)
                                         (RespGetBanTime banTime)   <- withSocket (CmdGetJailBanTime  jail)
                                         (RespGetMaxRetry maxRetry) <- withSocket (CmdGetJailMaxRetry jail)
                                         return (ARGetJail curFailed totalFailed fileList curBanned totalBanned
                                                           bannedIPs bannedASNs bannedCountries bannedRIRs
                                                           ignoreIP findTime banTime maxRetry)

            RespError _ e           -> return (ARError True e)
            e                       -> return (ARError True (T.pack $ "Got unexpected response: " ++ show e))

    maxretry :: Int -> Handler APIResponse
    maxretry r = withSocket (CmdSetJailMaxRetry jail r) >>= testSuccess

    findtime :: Seconds -> Handler APIResponse
    findtime t = withSocket (CmdSetJailFindTime jail t) >>= testSuccess

    bantime :: Seconds -> Handler APIResponse
    bantime t = withSocket (CmdSetJailBanTime jail t) >>= testSuccess

    addignore :: IPAddr -> Handler APIResponse
    addignore ip = withSocket (CmdSetJailAddIgnoreIP jail ip) >>= testSuccess

    delignore :: IPAddr -> Handler APIResponse
    delignore ip = withSocket (CmdSetJailDelIgnoreIP jail ip) >>= testSuccess

    ban :: IPAddr -> Handler APIResponse
    ban ip = withSocket (CmdSetJailBanIP jail [ip]) >>= testSuccess

    unban :: IPAddr -> Handler APIResponse
    unban ip = withSocket (CmdSetJailUnbanIP jail [ip]) >>= testSuccess
