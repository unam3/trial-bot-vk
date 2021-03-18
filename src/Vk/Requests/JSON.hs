{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Vk.Requests.JSON where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Vk.Types

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


newtype SendMessageResponse = SendMessageResponse {
    response :: Int
} deriving (Show, Generic)

instance FromJSON SendMessageResponse


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

