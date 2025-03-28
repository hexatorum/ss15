module Shared.Components.Damage( Damage(..), DamageType(..), DamageModifier(..) ) where

import Apecs

data DamageType =
    Slash | Blunt | Piercing -- Brute
  | Radiation | Caustic | Heat | Cold | Shock -- Burn
  | Poison -- Toxin
  | Integral -- Structural
  | Bloodloss | Asphyxiation -- Airloss
  | Nerve | Stamina | Genetic | Bleeding -- General
  deriving (Eq, Show)

data DamageModifier = DamageModifier DamageType Int deriving (Eq, Show)

type Source = Entity
newtype Damage = Damage [(DamageType, Float, Source)] deriving Show
instance Component Damage where type Storage Damage = Map Damage
