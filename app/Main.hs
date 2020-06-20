module Main where

import Bot (cycleEcho, Config)
import Control.Monad (void)
import Data.Text (pack)
import System.Environment (getArgs)
import System.Log.Logger (traplogging, Priority (ERROR))


processArgs :: [String] -> Maybe Config
processArgs [token, groupId, helpMsg, repeatMsg, echoRepeatNumberStr] = let {
    echoRepeatNumber = (read echoRepeatNumberStr :: Int);
    isInRange n = n > 0 && n < 6;
} in if or [null token, null helpMsg, null repeatMsg, not $ isInRange echoRepeatNumber]
    then Nothing
    else Just (
        pack token,
        pack groupId,
        pack helpMsg,
        pack repeatMsg,
        pack echoRepeatNumberStr
    )
processArgs _ = Nothing

trialBot :: IO ()
trialBot = do
    args <- getArgs
    case args of
        [_, _, _, _, _] -> case processArgs args of
            Just args' -> void $ cycleEcho args'
            Nothing -> error "error: some argument passed from command line is wrong"
        _ -> error "error: exactly five arguments needed: access token, group id, helpMsg, repeatMsg, echoRepeatNumber"

main :: IO ()
main = traplogging "trial-bot-vk.main" ERROR "Bot shutdown due to" trialBot
