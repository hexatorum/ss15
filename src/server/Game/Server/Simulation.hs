module Game.Server.Simulation (initWorld) where

import Apecs
import Game.Components

makeWorld "World" [''Player]
