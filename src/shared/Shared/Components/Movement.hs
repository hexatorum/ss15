module Shared.Components.Movement() where

import Apecs

data MovementState = 
    Running
  | Walking
  | Crouching
  | Proning
  deriving (Eq, Show)

type Speed = Float -- this is actually the interpolation speed and how long it takes before the mob can move again.
data Movable = Movable MovementState Speed deriving Show
instance Component Movable where type Storage Movable = Map Movable
