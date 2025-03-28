module Shared.Components.Atmos( Gas(..)
  , Mixture(..)
  , Reaction(..)
  , GasInstance(..)
  , AtmosContainer(..)
  , Temperature(..)
  , oxygen, nitrogen, hydrogen, waterVapor
  , hydrogenFire, waterVaporCondensation
  ) where

import Apecs

type Mol = Float

type Name = String
type MolecularIdentifier = String
type HeatCapacity = Float

data Gas = Gas Name MolecularIdentifier HeatCapacity deriving Eq

instance Show Gas where
  show (Gas x y _) = x ++ " (" ++ y ++ ")"

type Factor = Gas -- gas factors required for a gas reaction to be present.

data Reaction = Reaction Name [Factor] deriving (Eq, Show)

data GasInstance = GasInstance Gas Mol deriving Show

data Mixture = Mixture { volume :: Float -- liters
  , temperature :: Float -- kelvin
  , pressure :: Float -- pascals
  , energy :: Float -- joules
  , gases :: [GasInstance]
  , reactions :: [Reaction]
  } deriving Show

newtype AtmosContainer = AtmosContainer Mixture deriving Show
instance Component AtmosContainer where type Storage AtmosContainer = Map AtmosContainer

newtype Temperature = Temperature Float deriving Show
instance Component Temperature where type Storage Temperature = Map Temperature

data AtmosVoid = AtmosVoid deriving Show -- atmospherics flag to instantly wipe all gases in the mixture
instance Component AtmosVoid where type Storage AtmosVoid = Map AtmosVoid
-- we don't ACTUALLY need this for space, as the logic already checks if there are any tile layers available on that tile when trying to transfer to it
-- if there are any tile layers, it will check if gases can pass through that layer, if not, it'll successfully be able to transfer, if yes, it will void the gas.
-- this is just for things such as special kinds of items like vents and such.

-- TODO: add more GAS reactions, actually write the atmos system

oxygen :: Gas
oxygen = Gas "Oxygen" "O2" 0 -- no idea abt heat capacity

nitrogen :: Gas
nitrogen = Gas "Nitrogen" "N2" 0

hydrogen :: Gas
hydrogen = Gas "Hydrogen" "H2" 0

waterVapor :: Gas
waterVapor = Gas "Water Vapor" "H2O" 0

hydrogenFire :: Reaction
hydrogenFire = Reaction "Hydrogen Fire" [oxygen, hydrogen] -- Burning hydrogen becomes water.

waterVaporCondensation :: Reaction
waterVaporCondensation = Reaction "Water Vapor Condensation" [waterVapor] -- Water vapor, when below a certain arbitrary temperature at varying pressures, becomes normal water.
