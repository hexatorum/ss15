module Game.Components(
  ServerEntity, ClientEntity,
  Camera(..), Client(..), NetEntity(..),
  Player(..),
) where

import GHC.Generics(Generic)

import Codec.Serialise(Serialise)

import Apecs
import Apecs.Experimental.Reactive

import Data.Semigroup
import Data.Monoid
import Data.Text(Text)

import Linear

type ServerEntity = Entity
type ClientEntity = Entity

newtype Camera = Camera (V2 Float)
instance Component Camera where
  type Storage Camera = Global Camera
instance Semigroup Camera where (Camera p1) <> (Camera p2) = Camera $ (p1 ^+^ p2)
instance Monoid Camera where mempty = Camera $ V2 0 0

data Client = Client
instance Component Client where
  type Storage Client = Unique Client

newtype NetEntity = NetEntity Int
  deriving newtype (Eq, Ord, Show, Enum)
instance Component NetEntity where
  type Storage NetEntity = Reactive (EnumMap NetEntity) (Map NetEntity)

newtype Player = Player Text deriving Generic
instance Component Player where
  type Storage Player = Map Player
instance Serialise Player
