module Client.Game (runGame) where

import Apecs
import Linear
import Graphics.Rendering.OpenGL qualified as GL
import SDL qualified

import Components

makeWorld "World" [''Camera, ''Client]

type System' a = System World a

step :: Float -> System' ()
step dT = pure ()

runGame :: Float -> System' ()
runGame dT = set global $ Camera (V2 0.0 0.0)
