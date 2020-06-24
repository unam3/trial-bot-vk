{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Bot
    (
    cycleProcessing,
    Config
    ) where

import Data.Aeson (FromJSON, ToJSON (toJSON), defaultOptions, fieldLabelModifier, genericToJSON)
import Data.Text (Text, breakOn, drop)
import GHC.Generics (Generic)
import Prelude hiding (drop, id)
import qualified Prelude (drop)
import Network.HTTP.Req
import System.Log.Logger (Priority (DEBUG), debugM, setLevel, updateGlobalLogger)


type TokenSection = Text
type GroupId = Text
type HelpMessage = Text
type RepeatMessage = Text
type NumberOfRepeats = Text
type Config = (TokenSection, GroupId, HelpMessage, RepeatMessage, NumberOfRepeats)

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
getLongPollServerInfo (tokenSection, groupId, _, _, _) = let {
    urlScheme = https "api.vk.com" /: "method" /: "groups.getLongPollServer";
    params = "v" =: ("5.110" :: Text) <>
        "access_token" =: tokenSection <>
        "group_id" =: groupId;
    runReqM = req GET urlScheme NoReqBody jsonResponse params >>=
        return . response . responseBody :: Req LPServerInfo;
} in runReq defaultHttpConfig runReqM


-- create URL only once, not on each request!
-- makeLongPollURL :: LPServerInfo -> Text


data Update = Update {
    _type :: Text,
    --object :: Object,
    group_id :: GroupId
} deriving (Show, Generic)

instance ToJSON Update where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = Prelude.drop 1 }
instance FromJSON Update

data LPResponse = LPResponse {
    ts :: Text,
    updates :: [Update]
} deriving (Show, Generic)

instance ToJSON LPResponse
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

cycleProcessing' :: Config -> LPServerInfo -> IO LPResponse
cycleProcessing' config serverInfo =
    --debugM  "trial-bot-vk.bot" . show $ server serverInfo
    getLongPoll serverInfo
    -- >>= debugM  "trial-bot-vk.bot" . show
    >>= \ updates -> debugM  "trial-bot-vk.bot" (show updates)
    >> cycleProcessing' config serverInfo

cycleProcessing :: Config -> IO LPResponse
cycleProcessing config = updateGlobalLogger "trial-bot-vk.bot" (setLevel DEBUG)
    >> getLongPollServerInfo config
    >>= \ serverInfo -> debugM  "trial-bot-vk.bot" (show serverInfo)
    >> cycleProcessing' config serverInfo
