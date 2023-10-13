module Domain.Types where

type Host = String

type Port = Int

type PingTime = Int

data Timestamp = Timestamp
  deriving (Show, Eq, Ord)