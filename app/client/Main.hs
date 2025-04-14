module Main (main) where

import System.IO
import System.Exit

import Control.Monad (unless)

import Data.Maybe

import Apecs
import qualified SDL
import qualified SDL.Image (quit)
import qualified SDL.Font 

import qualified Graphics.Rendering.OpenGL.GL as GL

import Intent(Intent)
import qualified Intent
import qualified Direction

import Linear

action :: Intent -> IO ()
action Intent.Quit = exitSuccess
action _ = pure ()

getKey :: SDL.KeyboardEventData -> Maybe Intent
getKey (SDL.KeyboardEventData _ SDL.Released _ _) = Nothing
getKey (SDL.KeyboardEventData _ SDL.Pressed True _) = Nothing
getKey (SDL.KeyboardEventData _ SDL.Pressed False keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeEscape -> Intent.Quit
    SDL.KeycodeW -> Intent.Move Direction.Up
    SDL.KeycodeS -> Intent.Move Direction.Down
    SDL.KeycodeA -> Intent.Move Direction.Left
    SDL.KeycodeD -> Intent.Move Direction.Right
    _ -> Intent.Wait

mapKeysToIntent :: [SDL.Event] -> [Intent]
mapKeysToIntent = mapMaybe (payloadToIntent . SDL.eventPayload)
  where
    payloadToIntent :: SDL.EventPayload -> Maybe Intent
    payloadToIntent (SDL.KeyboardEvent k)       = getKey k
    payloadToIntent SDL.QuitEvent               = Just Quit
    payloadToIntent _                           = Nothing

loop :: IO () -> [Intent]
loop = do
  events <- SDL.pollEvents
  putStrLn events
  keys <- mapKeysToIntent events
  keys

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Test" $ SDL.defaultWindow {
    SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
  }
  context <- SDL.glCreateContext window


