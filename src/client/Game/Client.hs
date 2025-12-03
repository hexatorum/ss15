module Game.Client (runGame, runDraw, initGame, step, draw) where

import SDL qualified
import Apecs
import Linear

import Game
import Game.Client.World

initGame :: IO World
initGame = do
  world <- initWorld
  runWith world initialise
  pure world

initialise :: System' ()
initialise = set global $ Camera (V2 0.0 0.0)

step :: Float -> System' ()
step dT = pure ()

draw :: System' ()
draw = pure ()

runDraw :: World -> IO ()
runDraw world = runWith world draw

runGame :: Float -> World -> IO ()
runGame dT world = runWith world $ step dT

