module Network.Message(ClientMessage(..), ServerMessage(..), Ping(..)) where

import Intent
import Data.Text(Text)
import Codec.Serialise(Serialise)
import GHC.Generics(Generic)

data Ping = Ping | Pong
  deriving Generic

data ClientMessage a = Call Int a | Cast a
  deriving Generic

data ServerMessage a = Reply Int a | Event a
  deriving Generic

instance Serialise Ping
instance Serialise a => Serialise (ClientMessage a)
instance Serialise a => Serialise (ServerMessage a)
