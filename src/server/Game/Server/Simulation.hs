module Game.Server.Simulation (World, initWorld) where

import Apecs
import Game.Components

makeWorld "World" [''Player]

