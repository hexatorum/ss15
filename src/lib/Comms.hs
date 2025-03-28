module Comms( Channel(..), Message ) where

data Channel =
    Local
  | Whisper
  | Dead
  | Radio Float
  | Emote
  | AudibleEmote Float
  | OOC

type Message = (Channel, String)
