{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Bot
    (
    cycleEcho,
    Config
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (id)
--import qualified Prelude (id)
import Network.HTTP.Req
import System.Log.Logger (Priority (DEBUG), setLevel, updateGlobalLogger)


type TokenSection = Text
type GroupId = Text
type HelpMessage = Text
type RepeatMessage = Text
type NumberOfRepeats = Text
type Config = (TokenSection, GroupId, HelpMessage, RepeatMessage, NumberOfRepeats)

data LPServerInfoResponse = LPServerInfoResponse {
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

getLongPollKey :: Config -> IO LPServerInfo
getLongPollKey (tokenSection, groupId, helpMsg, _, _) = let {
    urlScheme = https "api.vk.com" /: "method" /: "groups.getLongPollServer";
    params = "v" =: ("5.110" :: Text) <>
        "access_token" =: tokenSection <>
        "group_id" =: groupId;
    runReqM = req GET urlScheme NoReqBody jsonResponse params >>=
        (\ response' -> return (response $ responseBody response' :: LPServerInfo));
} in runReq defaultHttpConfig runReqM

cycleEcho :: Config -> IO ()
cycleEcho config = updateGlobalLogger "trial-bot.bot" (setLevel DEBUG)
    >> getLongPollKey config
    >>= print
