{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Week05.Parser
    ( encode
    , decode
    , Transaction (..)
    , TId
    , FromJSON (..)
    , ToJSON (..)
    ) where

import Data.Aeson
    ( FromJSON (..)
    , KeyValue ((.=))
    , ToJSON (..)
    , decode
    , encode
    , object
    , withObject
    , (.:)
    )

type TId = String

data Transaction = Transaction
    { from :: String
    , to :: String
    , amount :: Integer
    , tid :: TId
    }
    deriving (Eq, Show)

instance FromJSON Transaction where
    parseJSON = withObject "Transaction" $ \o -> do
        Transaction
            <$> o .: "from"
            <*> o .: "to"
            <*> o .: "amount"
            <*> o .: "tid"

instance ToJSON Transaction where
    toJSON Transaction{..} =
        object
            [ "from" .= from
            , "to" .= to
            , "amount" .= amount
            , "tid" .= tid
            ]
