module Network.Message(ClientMessage(..), ServerMessage(..), Message(..)) where

import Data.Text(Text)
import Codec.Serialise(Serialise)
import GHC.Generics(Generic)

type ServerToClientEntity = Int

data Message =
    Ping | Pong
  | TryLogin Text Text | LoginSuccess ServerToClientEntity | LoginFail
    deriving Generic

instance Serialise Message

data ClientMessage a = Call Int a | Cast a
  deriving Generic

data ServerMessage a = Reply Int a | Event a
  deriving Generic

instance Serialise a => Serialise (ClientMessage a)
instance Serialise a => Serialise (ServerMessage a)
