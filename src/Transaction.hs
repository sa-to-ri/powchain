{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Transaction where

import           GHC.Generics

import           Text.Read                (readMaybe)

import           Data.Text.Lazy           (Text)
import qualified Data.Text.Lazy        as Text
import           Data.Aeson
import           Data.Time.Clock.POSIX    (POSIXTime, getPOSIXTime)

data Transaction = Transaction
  { timestamp     :: POSIXTime
  , sender        :: Text
  , recipient     :: Text
  , amount        :: Int
  } deriving (Generic, Show, Eq, Ord)

instance FromJSON Transaction
instance ToJSON Transaction

mkTransaction_ :: POSIXTime -> Text -> Text -> Int -> Transaction
mkTransaction_ timestamp sender recipient amount = Transaction
  { timestamp = timestamp
  , sender    = sender
  , recipient = recipient
  , amount    = amount
  }

mkTransaction :: Text -> Text -> Text -> IO (Maybe Transaction)
mkTransaction sender recipient amount = do
  timestamp <- getPOSIXTime

  pure $ mkTransaction_ timestamp sender recipient <$> readMaybe (Text.unpack amount)

