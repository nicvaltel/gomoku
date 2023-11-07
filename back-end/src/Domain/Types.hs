module Domain.Types where

import Data.ByteString (ByteString)
import Data.Text (Text)

type Host = String

type Port = Int

type PingTime = Int

type Password = Text

data Timestamp = Timestamp
  deriving (Show, Eq, Ord)

type MessageStr = ByteString