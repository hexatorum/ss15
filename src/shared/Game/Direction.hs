module Game.Direction(Direction(..)) where

import Codec.Serialise(Serialise)
import GHC.Generics(Generic)

data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Show, Enum, Generic)

instance Serialise Direction
