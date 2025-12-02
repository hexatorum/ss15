module Game.Intent(Intent(..)) where

import Apecs
import Codec.Serialise(Serialise)
import GHC.Generics(Generic)
import Game.Direction(Direction)

data Intent =
    Quit
  | Wait
  | Move Direction
  deriving (Eq, Show, Generic)

instance Serialise Intent
