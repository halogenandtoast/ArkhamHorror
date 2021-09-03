module Arkham.Types.Investigator.Cards.DexterDrake where

import Arkham.Prelude

import Arkham.Types.Investigator.Attrs

newtype DexterDrake = DexterDrake InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

dexterDrake :: DexterDrake
dexterDrake = DexterDrake $ baseAttrs
  "98016"
  "Dexter Drake"
  Mystic
  Stats
    { health = 6
    , sanity = 8
    , willpower = 5
    , intellect = 2
    , combat = 3
    , agility = 2
    }
  [Sorcerer, Veteran]

instance (InvestigatorRunner env) => RunMessage env DexterDrake where
  runMessage msg (DexterDrake attrs) = DexterDrake <$> runMessage msg attrs
