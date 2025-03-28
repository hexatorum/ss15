module Shared.Components.Tiles( TileLayers(..) ) where

import Apecs

type Name = String

data TileLayer = TileLayer Name Float deriving (Eq, Show)

lattice :: TileLayer
lattice = TileLayer "Lattice" 50

plating :: TileLayer
plating = TileLayer "Plating" 50

data TileLayers = TileLayers [TileLayer] deriving Show
instance Component TileLayers where type Storage TileLayers = Map TileLayers
-- contributes to the tile's maxhealth before it turns into space
-- example: [lattice, steel_tile], [lattice, plating, plating, steel_tile]
-- the lattice adds up to 50% health boost, plating finishes with another 50%.
-- tiles with no support will have the default 5 health, which instantly breaks them.
