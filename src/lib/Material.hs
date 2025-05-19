module Material(iron, glass) where

import Shared.Components.Damage

type Name = String
data Material = Material Name [DamageModifier] Float deriving (Eq, Show)

iron :: Material
iron = Material "Iron"
  [ DamageModifier Blunt 80
  , DamageModifier Slash 80
  , DamageModifier Pierce 80
  ] 1.25

glass :: Material
glass = Material "Glass"
  [ DamageModifier Blunt 140
  , DamageModifier Slash 140
  , DamageModifier Pierce 140
  ] 1.05
