{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Vk
    (
    startBotWithLogger
    ) where

import Control.Monad (replicateM_, void)
import Data.Text (Text, pack)
import Data.Text.Read (decimal)
import Data.Time.Clock.System (getSystemTime)
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import Prelude hiding (drop, id)
import System.Exit (exitFailure, exitSuccess)
import System.Log.Logger (Priority (DEBUG, ERROR), debugM, errorM, setLevel, traplogging, updateGlobalLogger)

import Vk.Requests
import Vk.Requests.JSON
import Vk.Types


isMessageNew :: Update -> Bool
isMessageNew = (== "message_new") . (_type :: Update -> Text)

getInt :: Text -> Int
getInt = fst . fromRight (1, "1") . decimal

processUpdates :: Config -> [Update] -> IO Config
processUpdates config@(tokenSection, groupId, helpMsg, repeatMsg, echoRepeatNumberText, numberOfRepeatsMap) updates' = let {
    newMessages = filter isMessageNew updates';
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
    >>= \ serverInfo -> debugM "trial-bot-vk.bot" (show serverInfo)
    >> cycleProcessing' config serverInfo


processArgs :: [String] -> Either String Config
processArgs [token, groupId, helpMsg, repeatMsg, echoRepeatNumberStr] = let {
    echoRepeatNumber = (read echoRepeatNumberStr :: Int);
    isInRange n = n > 0 && n < 6;
} in if or [null token, null groupId, null helpMsg, null repeatMsg, not $ isInRange echoRepeatNumber]
    then Left "Some argument passed from command line is wrong."
    else Right (
        pack token,
        pack groupId,
        pack helpMsg,
        pack repeatMsg,
        pack echoRepeatNumberStr,
        M.empty
    )
processArgs _ = Left "Exactly five arguments needed: access token, group id, helpMsg, repeatMsg, echoRepeatNumber."

startBot :: [String] -> IO ()
startBot args =
    case processArgs args of
        Right config -> void $ cycleProcessing config
            >> exitSuccess
        Left errorMessage -> errorM "trial-bot-vk.bot" errorMessage
            >> exitFailure

startBotWithLogger :: [String] -> IO ()
startBotWithLogger args = traplogging "trial-bot-vk.bot" ERROR "Bot shutdown due to" $ startBot args
