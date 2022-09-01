{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Fail2banHelper where

import qualified Data.Aeson as JSON
import Data.Aeson.TH
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Language.Python.Pickle as P
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as S

import Text.Printf (printf)

f2bProtoEnd :: BS.ByteString
f2bProtoEnd = T.encodeUtf8 "<F2B_END_COMMAND>"

f2bProtoClose :: BS.ByteString
f2bProtoClose = T.encodeUtf8 "<F2B_CLOSE_COMMAND>"

emptyMsg :: BS.ByteString
emptyMsg = T.encodeUtf8 ""

packetBufSize :: Int
packetBufSize = 16384


data LogLevel =
    LLCritical
  | LLError
  | LLWarning
  | LLNotice
  | LLInfo

data SyslogSocket =
    SLAuto
  | SLSocket FilePath

data Action =
    CommandAction
      { caName :: T.Text }
  | PythonAction
      { paName :: T.Text
      , paFile :: FilePath
      , paKWArgs :: JSON.Object
      }

type Jail = T.Text
type IPAddr = T.Text
type Seconds = Int

data F2BCommand =
  -- Basic
    CmdStart
  | CmdRestart
  | CmdRestartJail
    { cRestartJail :: Jail
    , cRestartIfExists :: Bool
    , cRestartUnban :: Bool
    }
  | CmdReload
    { cReloadAll :: Bool
    , cReloadRestart :: Bool
    , cReloadUnban :: Bool
    }
  | CmdReloadJail
    { cReloadJJail :: Jail
    , cReloadJRestart :: Bool
    , cReloadJIfExists :: Bool
    , cReloadJUnban :: Bool
    }
  | CmdStop
  | CmdUnbanAll
  | CmdUnban { cUnbanIps :: [IPAddr] }
  | CmdBanned
  | CmdBannedJails { cBannedJIps :: [IPAddr] }
  | CmdStatus
  | CmdPing
  | CmdEcho
  | CmdHelp
  | CmdVersion
  -- Logging
  | CmdSetLogLevel { cSLogLevel :: LogLevel }
  | CmdGetLogLevel
  | CmdGetLogTarget
  | CmdSetSyslogSocket { cSSyslogSocket :: SyslogSocket }
  | CmdGetSyslogSocket
  | CmdFlushLogs
  | CmdSetDBFile { cSDBFile :: FilePath }
  | CmdGetDBFile
  | CmdSetDBMaxMatches { cSDBMaxMatches :: Int}
  | CmdGetDBMaxMatches
  | CmdSetDBPurgeAge { cSDBPurgeAge :: Seconds }
  | CmdGetDBPurgeAge
  -- Jail Control
  | CmdAddJail
    { cAddJName :: Jail
    , cAddJBackend :: T.Text
    }
  | CmdStartJail { cStartJail :: Jail }
  | CmdStopJail { cStopJail :: Jail }
  | CmdStatusJailCymru { cStatusJail :: Jail }
  -- Jail Configuration
  | CmdSetJailIdle
    { cSJailIdleJail :: Jail
    , cSJailIdle :: Bool
    }
  | CmdSetJailIgnoreSelf
    { cSJailIgnoreSelfJail :: Jail
    , cSJailIgnoreSelf :: Bool
    }
  | CmdSetJailAddIgnoreIP
    { cSAddIgnoreIpJail :: Jail
    , cSAddIgnoreIP :: IPAddr
    }
  | CmdSetJailDelIgnoreIP
    { cSDelIgnoreIpJail :: Jail
    , cSDelIgnoreIP :: IPAddr
    }
  -- External command that accepts an IP as its argument and returns
  -- true (exit code 0) if it is to be ignored.
  | CmdSetJailIgnoreCommand
    { cSIgnoreCommandJail :: Jail
    , cSIgnoreCommand :: FilePath
    }
  -- I don't know what this does in particular.
  | CmdSetJailIgnoreCache
    { cSIgnoreCacheJail :: Jail
    , cSIgnoreCache :: T.Text
    }
  | CmdSetJailAddLogPath
    { cSAddLogPathJail :: Jail
    , cSAddLogPathFile :: FilePath
    , cSAddLogPathTail :: Bool
    }
  | CmdSetJailDelLogPath
    { cSDelLogPathJail :: Jail
    , cSDelLogPathFile :: FilePath
    }
  | CmdSetJailLogEncoding
    { cSLogEncodingJail :: Jail
    , cSLogEncoding :: T.Text
    }
  | CmdSetJailAddJournalMatch
    { cSAddJournalMatchJail :: Jail
    , cSAddJournalMatch :: T.Text
    }
  | CmdSetJailDelJournalMatch
    { cSDelJournalMatchJail :: Jail
    , cSDelJournalMatch :: T.Text
    }
  | CmdSetJailAddFailRegex
    { cSAddFailRegexJail :: Jail
    , cSAddFailRegex :: T.Text
    }
  | CmdSetJailDelFailRegex
    { cSDelFailRegexJail :: Jail
    , cSDelFailRegexIndex :: Int
    }
  | CmdSetJailAddIgnoreRegex
    { cSAddIgnoreRegexJail :: Jail
    , cSAddIgnoreRegex :: T.Text
    }
  | CmdSetJailDelIgnoreRegex
    { cSDelIgnoreRegexJail :: Jail
    , cSDelIgnoreRegexIndex :: Int
    }
  | CmdSetJailFindTime
    { cSFindTimeJail :: Jail
    , cSFindTime :: Seconds
    }
  | CmdSetJailBanTime
    { cSBanTimeJail :: Jail
    , cSBanTime :: Seconds
    }
  | CmdSetJailDatePattern
    { cSDatePatternJail :: Jail
    , cSDatePattern :: T.Text
    }
  | CmdSetJailUseDns
    { cSUseDnsJail :: Jail
    , cSUseDns :: Bool
    }
  | CmdSetJailAttempt
    { cSAttemptJail :: Jail
    , cSAttemptIP :: IPAddr
    }
  | CmdSetJailBanIP
    { cSBanIPJail :: Jail
    , cSBanIPs :: [IPAddr]
    }
  | CmdSetJailUnbanIP
    { cSUnbanIPJail :: Jail
    , cSUnbanIPs :: [IPAddr]
    }
  | CmdSetJailMaxRetry
    { cSMaxRetryJail :: Jail
    , cSMaxRetry :: Int
    }
  | CmdSetJailMaxMatches
    { cSMaxMatchesJail :: Jail
    , cSMaxMatches :: Int
    }
  | CmdSetJailMaxLines
    { cSMaxLinesJail :: Jail
    , cSMaxLines :: Int
    }
  | CmdSetJailAddAction
    { cSAddActionJail :: Jail
    , cSAddAction :: Action
    }
  | CmdSetJailDelAction
    { cSDelActionJail :: Jail
    , cSDelAction :: T.Text
    }
  -- Command Action Configuration
  | CmdSetJailActionStart
    { cSActionStartJail :: Jail
    , cSActionStartAction :: T.Text
    , cSActionStartCmd :: T.Text
    }
  | CmdSetJailActionStop
    { cSActionStopJail :: Jail
    , cSActionStopAction :: T.Text
    , cSActionStopCmd :: T.Text
    }
  | CmdSetJailActionCheck
    { cSActionCheckJail :: Jail
    , cSActionCheckAction :: T.Text
    , cSActionCheckCmd :: T.Text
    }
  | CmdSetJailActionBan
    { cSActionBanJail :: Jail
    , cSActionBanAction :: T.Text
    , cSActionBanCmd :: T.Text
    }
  | CmdSetJailActionUnban
    { cSActionUnbanJail :: Jail
    , cSActionUnbanAction :: T.Text
    , cSActionUnbanCmd :: T.Text
    }
  | CmdSetJailActionTimeout
    { cSActionTimeoutJail :: Jail
    , cSActionTimeoutAction :: T.Text
    , cSActionTimeout :: Seconds
    }
  -- General Action Configuration
  | CmdSetJailActionProperty
    { cSActionPropertyJail :: Jail
    , cSActionPropertyAction :: T.Text
    , cSActionProperty :: T.Text
    , cSActionPropertyValue :: T.Text
    }
  | CmdSetJailActionMethod
    { cSActionMethodJail :: Jail
    , cSActionMethodAction :: T.Text
    , cSActionMethod :: T.Text
    , cSActionMethodKWArgs :: Maybe JSON.Object
    }
  -- Jail Information
  | CmdGetJailBanned { cGJailBanned :: Jail }
  | CmdGetJailBannedIP
    { cGJailBanned :: Jail
    , cGJailBannedIP :: IPAddr
    }
  | CmdGetJailLogPath { cGJailLogPath :: Jail }
  | CmdGetJailLogEncoding { cGJailLogEncoding :: Jail }
  | CmdGetJailJournalMatch { cGJailJournalMatch :: Jail }
  | CmdGetJailIgnoreSelf { cGJailIgnoreSelf :: Jail }
  | CmdGetJailIgnoreIP { cGJailIgnoreIP :: Jail }
  | CmdGetJailIgnoreCommand { cGJailIgnoreCommand :: Jail }
  | CmdGetJailFailRegex { cGJailFailRegex :: Jail }
  | CmdGetJailIgnoreRegex { cGJailIgnoreRegex :: Jail }
  | CmdGetJailFindTime { cGJailFindTime :: Jail }
  | CmdGetJailBanTime { cGJailBanTime :: Jail }
  | CmdGetJailDatePattern { cGJailDatePattern :: Jail }
  | CmdGetJailUseDns { cGJailUseDns :: Jail }
  | CmdGetJailBanIP
    { cGJailBanIPJail :: Jail
    , cGJailBanIPSepOrWithTime :: Either Char Bool
    }
  | CmdGetJailMaxRetry { cGJailMaxRetry :: Jail }
  | CmdGetJailMaxMatches { cGJailMaxMatches :: Jail }
  | CmdGetJailMaxLines { cGJailMaxLines :: Jail }
  | CmdGetJailActions { cGJailActions :: Jail }
  -- Command Action Information
  | CmdGetJailActionStart
    { cGJailActionStartJail :: Jail
    , cGJailActionStartAction :: T.Text
    }
  | CmdGetJailActionStop
    { cGJailActionStopJail :: Jail
    , cGJailActionStopAction :: T.Text
    }
  | CmdGetJailActionCheck
    { cGJailActionCheckJail :: Jail
    , cGJailActionCheckAction :: T.Text
    }
  | CmdGetJailActionBan
    { cGJailActionBanJail :: Jail
    , cGJailActionBanAction :: T.Text
    }
  | CmdGetJailActionUnban
    { cGJailActionUnbanJail :: Jail
    , cGJailActionUnbanAction :: T.Text
    }
  | CmdGetJailActionTimeout
    { cGJailActionTimeoutJail :: Jail
    , cGJailActionTimeoutAction :: T.Text
    }
  -- General Action Information
  | CmdGetJailActionProperties
    { cGJailActionPropertiesJail :: Jail
    , cGJailActionPropertiesAction :: T.Text
    }
  | CmdGetJailActionMethods
    { cGJailActionMethodsJail :: Jail
    , cGJailActionMethodsAction :: T.Text
    }
  | CmdGetJailActionProperty
    { cGJailActionPropertyJail :: Jail
    , cGJailActionPropertyAction :: T.Text
    , cGJailActionProperty :: T.Text
    }


data F2BResponse =
    RespError
    { f2brError  :: Bool
    , f2brReason :: T.Text
    }
  | RespVersion { f2brVersion :: T.Text }
  | RespLogTarget { f2brLogTarget :: T.Text }
  | RespDBFile { f2brDBFile :: T.Text }
  | RespDBPurgeAge { f2brDBPurgeAge :: Seconds }
  | RespStatus { f2brStatus :: ((T.Text, Int), (T.Text, T.Text)) }
  | RespUnban
  | RespBanned { f2brBanned :: [M.Map Jail [IPAddr]] }
  | RespReload
  | RespSetSuccess
  | RespStatusJailCymru { f2brCymruStatus :: CymruStatus }
  | RespGetIgnoreIP { f2brIgnoreIPs :: [IPAddr] }
  | RespGetFindTime { f2brFindTime :: Seconds }
  | RespGetBanTime { f2brBanTime :: Seconds }
  | RespGetMaxRetry { f2brMaxRetry :: Int }
  deriving (Eq, Show)

type CurrentlyFailed = (T.Text, Int)
type TotalFailed = (T.Text, Int)
type FileList = (T.Text, [T.Text])
type Filter = (T.Text, (CurrentlyFailed, TotalFailed, FileList))

type CurrentlyBanned = (T.Text, Int)
type TotalBanned = (T.Text, Int)
type BannedIPs = (T.Text, [T.Text])
type BannedASNs = (T.Text, [T.Text])
type BannedCountries = (T.Text, [T.Text])
type BannedRIRs = (T.Text, [T.Text])
type Actions = (T.Text, (CurrentlyBanned, TotalBanned, BannedIPs, BannedASNs, BannedCountries, BannedRIRs))

type CymruStatus = (Filter, Actions)


cmd2Pickle :: [T.Text] -> BS.ByteString
cmd2Pickle cmd = P.pickle (P.List (map (P.BinString . T.encodeUtf8) cmd))

recvUntilEnd :: S.Socket -> BS.ByteString -> IO BS.ByteString
recvUntilEnd s initialMsg = do
    recvd <- S.recv s packetBufSize
    let msg = initialMsg `BS.append` recvd
    if f2bProtoEnd `BS.isSuffixOf` msg
       then case BS.stripSuffix f2bProtoEnd msg of
              Just msg' -> return msg'
              Nothing   -> error "This should never happen, error in recvUntilEnd"
       else recvUntilEnd s msg

bs2hex :: BS.ByteString -> T.Text
bs2hex = BS.foldr (\b -> (<>) (T.pack $ printf "%02x" b)) ""

handleNetworkSendRecv :: S.Socket -> [T.Text] -> (P.Value -> IO F2BResponse) -> IO F2BResponse
handleNetworkSendRecv s cmd success = do
    let pickled = cmd2Pickle cmd
    S.sendAll s pickled
    S.sendAll s f2bProtoEnd
    pickledRes <- recvUntilEnd s emptyMsg
    case P.unpickle pickledRes of
          Left e -> return (RespError True (T.pack ("Unpickle error: " ++ e)))
          Right val -> success val

handleNetworkSend :: S.Socket -> [T.Text] -> IO ()
handleNetworkSend s cmd = do
    let pickled = cmd2Pickle cmd
    S.sendAll s pickled
    S.sendAll s f2bProtoEnd

respError :: (Show a) => a -> F2BResponse
respError e = RespError True (T.pack ("Invalid response: " ++ show e))

sendCommand :: S.Socket -> F2BCommand -> IO F2BResponse
sendCommand s CmdVersion = do
    handleNetworkSendRecv s ["version"]
        (\val -> case (P.fromVal val :: Maybe (Int, T.Text)) of 
             Just (_, v) -> return (RespVersion v)
             Nothing -> return (respError val))

sendCommand s CmdGetLogTarget = do
    handleNetworkSendRecv s ["get", "logtarget"]
        (\val -> case (P.fromVal val :: Maybe (Int, T.Text)) of 
             Just (_, f) -> return (RespLogTarget f)
             Nothing -> return (respError val))

sendCommand s CmdGetDBFile = do
    handleNetworkSendRecv s ["get", "dbfile"]
        (\val -> case (P.fromVal val :: Maybe (Int, T.Text)) of 
             Just (_, f) -> return (RespDBFile f)
             Nothing -> return (respError val))

sendCommand s CmdGetDBPurgeAge = do
    handleNetworkSendRecv s ["get", "dbpurgeage"]
        (\val -> case (P.fromVal val :: Maybe (Int, Int)) of 
             Just (_, i) -> return (RespDBPurgeAge i)
             Nothing -> return (respError val))

sendCommand s CmdStatus = do
    handleNetworkSendRecv s ["status"]
        (\val -> case (P.fromVal val :: Maybe (Int, ((T.Text, Int), (T.Text, T.Text)))) of
             Just (_, s) -> return (RespStatus s)
             Nothing -> return (respError val))

sendCommand s (CmdUnban (ip:_)) =
    handleNetworkSend s ["unban", ip] >> return RespUnban
sendCommand _ (CmdUnban []) = return (RespError True (T.pack "Not enough arguments to unban"))

sendCommand s CmdBanned = do
    handleNetworkSendRecv s ["banned"]
        (\val -> case (P.fromVal val :: Maybe (Int, [M.Map T.Text [T.Text]])) of
             Just (_, m) -> return (RespBanned m)
             Nothing -> return (respError val))

sendCommand s (CmdStatusJailCymru jail) = do
    handleNetworkSendRecv s ["status", jail, "cymru"]
        (\val -> case (P.fromVal val :: Maybe (Int, CymruStatus)) of
             Just (_, m) -> return (RespStatusJailCymru m)
             Nothing -> return (respError val))

sendCommand s (CmdGetJailIgnoreIP jail) = do
    handleNetworkSendRecv s ["get", jail, "ignoreip"]
        (\val -> case (P.fromVal val :: Maybe (Int, [IPAddr])) of
             Just (_, m) -> return (RespGetIgnoreIP m)
             Nothing -> return (respError val))

sendCommand s (CmdGetJailFindTime jail) = do
    handleNetworkSendRecv s ["get", jail, "findtime"]
        (\val -> case (P.fromVal val :: Maybe (Int, Seconds)) of
             Just (_, m) -> return (RespGetFindTime m)
             Nothing -> return (respError val))

sendCommand s (CmdGetJailBanTime jail) = do
    handleNetworkSendRecv s ["get", jail, "bantime"]
        (\val -> case (P.fromVal val :: Maybe (Int, Seconds)) of
             Just (_, m) -> return (RespGetBanTime m)
             Nothing -> return (respError val))

sendCommand s (CmdGetJailMaxRetry jail) = do
    handleNetworkSendRecv s ["get", jail, "maxretry"]
        (\val -> case (P.fromVal val :: Maybe (Int, Int)) of
             Just (_, m) -> return (RespGetMaxRetry m)
             Nothing -> return (respError val))

sendCommand s (CmdSetJailMaxRetry jail r) = do
    handleNetworkSend s ["set", jail, "maxretry", T.pack $ show r] >> return RespSetSuccess

sendCommand s (CmdSetJailFindTime jail t) = do
    handleNetworkSend s ["set", jail, "findtime", T.pack $ show t] >> return RespSetSuccess

sendCommand s (CmdSetJailBanTime jail t) = do
    handleNetworkSend s ["set", jail, "bantime", T.pack $ show t] >> return RespSetSuccess

sendCommand s (CmdSetJailAddIgnoreIP jail ip) = do
    handleNetworkSend s ["set", jail, "addignoreip", ip] >> return RespSetSuccess

sendCommand s (CmdSetJailDelIgnoreIP jail ip) = do
    handleNetworkSend s ["set", jail, "delignoreip", ip] >> return RespSetSuccess

sendCommand s (CmdSetJailBanIP jail ips) = do
    handleNetworkSend s (["set", jail, "banip"] ++ ips) >> return RespSetSuccess

sendCommand s (CmdSetJailUnbanIP jail ips) = do
    handleNetworkSend s (["set", jail, "unbanip"] ++ ips) >> return RespSetSuccess

