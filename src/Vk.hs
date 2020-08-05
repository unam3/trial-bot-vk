{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Vk
    (
    startBotWithLogger
    ) where

import Control.Monad (replicateM_, void)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), defaultOptions, encode, fieldLabelModifier, genericParseJSON, genericToJSON)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text, breakOn, drop, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read (decimal)
import Data.Time.Clock.System (SystemTime, getSystemTime, systemNanoseconds)
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import GHC.Generics (Generic)
import Prelude hiding (drop, id)
import qualified Prelude (drop)
import Network.HTTP.Req
import System.Log.Logger (Priority (DEBUG, ERROR), debugM, setLevel, traplogging, updateGlobalLogger)


type TokenSection = Text
type GroupId = Text
type HelpMessage = Text
type RepeatMessage = Text
type NumberOfRepeats = Text
type FromId = Int
type NumberOfRepeatsMap = M.Map FromId NumberOfRepeats
type Config = (TokenSection, GroupId, HelpMessage, RepeatMessage, NumberOfRepeats, NumberOfRepeatsMap)

newtype LPServerInfoResponse = LPServerInfoResponse {
    response :: LPServerInfo
} deriving (Show, Generic)

instance FromJSON LPServerInfoResponse

data LPServerInfo = LPServerInfo {
    key :: Text,
    server :: Text,
    ts :: Text
} deriving (Show, Generic)

instance ToJSON LPServerInfo
instance FromJSON LPServerInfo

getLongPollServerInfo :: Config -> IO LPServerInfo
getLongPollServerInfo (tokenSection, groupId, _, _, _, _) = let {
    urlScheme = https "api.vk.com" /: "method" /: "groups.getLongPollServer";
    params = "v" =: ("5.110" :: Text) <>
        "access_token" =: tokenSection <>
        "group_id" =: groupId;
    runReqM = req GET urlScheme NoReqBody jsonResponse params >>=
        return . (response :: LPServerInfoResponse -> LPServerInfo) . responseBody :: Req LPServerInfo;
} in runReq defaultHttpConfig runReqM


data PrivateMessage = PrivateMessage {
    text :: Text,
    peer_id :: Int,
    date :: Int,
    payload :: Maybe Text,
    from_id :: FromId
} deriving (Show, Generic)

instance FromJSON PrivateMessage

newtype Object = Object {
    message :: PrivateMessage
} deriving (Show, Generic)

instance FromJSON Object

data Update = Update {
    _type :: Text,
    _object :: Object,
    _group_id :: Int
} deriving (Show, Generic)

instance FromJSON Update where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = Prelude.drop 1}

data LPResponse = LPResponse {
    ts :: Text,
    updates :: [Update]
} deriving (Show, Generic)

instance FromJSON LPResponse

getLongPoll :: LPServerInfo -> IO LPResponse
getLongPoll serverInfo = let {
    -- https://lp.vk.com/wh123456789
    (serverName, wh) = breakOn "/" $ drop 8 (server serverInfo);
    urlScheme = https serverName /: drop 1 wh;
    params = "act" =: ("a_check" :: Text) <>
        "key" =: key serverInfo <>
        "ts" =: (ts :: LPServerInfo -> Text) serverInfo <>
        "wait" =: ("25" :: Text);
    runReqM = req GET urlScheme NoReqBody jsonResponse params >>=
        return . responseBody :: Req LPResponse;
} in runReq defaultHttpConfig runReqM

isMessageNew :: Update -> Bool
isMessageNew = (== "message_new") . (_type :: Update -> Text)

newtype SendMessageResponse = SendMessageResponse {
    response :: Int
} deriving (Show, Generic)

instance FromJSON SendMessageResponse

getMessage :: Update -> PrivateMessage
getMessage = (message :: Object -> PrivateMessage) . _object;

isMsgTextHelpCommand :: Text -> Bool
isMsgTextHelpCommand = (== "/help")

isMsgTextRepeatCommand :: Text -> Bool
isMsgTextRepeatCommand = (== "/repeat")


data Action = Action {
    _label :: Text,
    _type :: Text,
    _payload :: Text
} deriving (Show, Generic)

instance ToJSON Action where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = Prelude.drop 1}

newtype Button = Button {
    action :: Action
} deriving (Show, Generic)

instance ToJSON Button

data Keyboard = Keyboard {
    one_time :: Bool,
    buttons :: [[Button]]
} deriving (Show, Generic)

instance ToJSON Keyboard

keyboard :: Text
keyboard = decodeUtf8 . toStrict $ encode Keyboard {
    one_time = True,
    buttons = [
        [Button { action = Action {_label = "1", _payload = "1", _type = "text"}}],
        [Button { action = Action {_label = "2", _payload = "2", _type = "text"}}],
        [Button { action = Action {_label = "3", _payload = "3", _type = "text"}}],
        [Button { action = Action {_label = "4", _payload = "4", _type = "text"}}],
        [Button { action = Action {_label = "5", _payload = "5", _type = "text"}}]
    ]}

sendMessage :: Config -> Update -> SystemTime -> IO SendMessageResponse
sendMessage (tokenSection, _, helpMsg, repeatMsg, echoRepeatNumberText, _) update systemTime = let {
    incomingMessage = getMessage update;
    urlScheme = https "api.vk.com" /: "method" /: "messages.send";
    msgText' = text incomingMessage;
    msgText
        | isMsgTextHelpCommand msgText' = helpMsg
        | isMsgTextRepeatCommand msgText' =
            mconcat ["Current number of repeats for you is ", echoRepeatNumberText, ". ", repeatMsg]
        | otherwise = msgText';
    params' = "v" =: ("5.110" :: Text) <>
        "access_token" =: tokenSection <>
        "group_id" =: _group_id update <>
        "peer_id" =: peer_id incomingMessage <>
        "random_id" =: pack (show $ systemNanoseconds systemTime) <>
        "message" =: msgText;
    params = if isMsgTextRepeatCommand msgText'
        then params' <> "keyboard" =: keyboard
        else params';
    runReqM = req GET urlScheme NoReqBody jsonResponse params
        >>= return . responseBody :: Req SendMessageResponse;
} in runReq defaultHttpConfig runReqM

getInt :: Text -> Int
getInt = fst . fromRight (1, "1") . decimal

processUpdates :: Config -> [Update] -> IO Config
processUpdates config@(tokenSection, groupId, helpMsg, repeatMsg, echoRepeatNumberText, numberOfRepeatsMap) updates = let {
    newMessages = filter isMessageNew updates;
    latestMessage = last newMessages;
    msg = getMessage latestMessage;
    msgText = text msg;
    isMsgHasNoText = msgText == "";
    maybeNewEchoRepeatNumberText = payload msg;
    newNumberOfRepeatsMap = M.insert (from_id msg) (fromJust maybeNewEchoRepeatNumberText) numberOfRepeatsMap;
    echoRepeatNumber = getInt $ M.findWithDefault echoRepeatNumberText (from_id msg) numberOfRepeatsMap;
    sendAndLog = getSystemTime
        >>= sendMessage config latestMessage
        >>= (debugM "trial-bot-vk.bot" . show);
    newConfig = (tokenSection, groupId, helpMsg, repeatMsg, echoRepeatNumberText, newNumberOfRepeatsMap);
} in if null newMessages || isMsgHasNoText
    then return config 
    else if isJust maybeNewEchoRepeatNumberText
    then return newConfig
    else (if isMsgTextHelpCommand msgText || isMsgTextRepeatCommand msgText
    then sendAndLog
    else replicateM_ echoRepeatNumber sendAndLog) >> return config

cycleProcessing' :: Config -> LPServerInfo -> IO LPResponse
cycleProcessing' config serverInfo =
    getLongPoll serverInfo
    >>= \ lp -> debugM "trial-bot-vk.bot" (show lp)
    >> processUpdates config (updates lp)
    >>= \ newConfig -> cycleProcessing' newConfig LPServerInfo {
        key = key serverInfo,
        server = server serverInfo,
        ts = (ts :: LPResponse -> Text) lp
    }

cycleProcessing :: Config -> IO LPResponse
cycleProcessing config = updateGlobalLogger "trial-bot-vk.bot" (setLevel DEBUG)
    >> getLongPollServerInfo config
    >>= \ serverInfo -> debugM  "trial-bot-vk.bot" (show serverInfo)
    >> cycleProcessing' config serverInfo


processArgs :: [String] -> Maybe Config
processArgs [token, groupId, helpMsg, repeatMsg, echoRepeatNumberStr] = let {
    echoRepeatNumber = (read echoRepeatNumberStr :: Int);
    isInRange n = n > 0 && n < 6;
} in if or [null token, null groupId, null helpMsg, null repeatMsg, not $ isInRange echoRepeatNumber]
    then Nothing
    else Just (
        pack token,
        pack groupId,
        pack helpMsg,
        pack repeatMsg,
        pack echoRepeatNumberStr,
        M.empty
    )
processArgs _ = Nothing

startBot :: [String] -> IO ()
startBot args =
    case args of
        [_, _, _, _, _] -> case processArgs args of
            Just args' -> void $ cycleProcessing args'
            Nothing -> error "error: some argument passed from command line is wrong"
        _ -> error "error: exactly five arguments needed: access token, group id, helpMsg, repeatMsg, echoRepeatNumber"

startBotWithLogger :: [String] -> IO ()
startBotWithLogger args = traplogging "trial-bot-vk.main" ERROR "Bot shutdown due to" $ startBot args
