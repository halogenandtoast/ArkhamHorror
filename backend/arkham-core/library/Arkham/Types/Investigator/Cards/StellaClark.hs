module Arkham.Types.Investigator.Cards.StellaClark where

import Arkham.Prelude

import Arkham.Types.Investigator.Attrs

newtype StellaClark = StellaClark InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

stellaClark :: StellaClark
stellaClark = StellaClark $ baseAttrs
  "60501"
  "Stella Clark"
  Survivor
  Stats
    { health = 8
    , sanity = 8
    , willpower = 3
    , intellect = 2
    , combat = 3
    , agility = 4
    }
  [Chosen, Civic]

instance (InvestigatorRunner env) => RunMessage env StellaClark where
  runMessage msg (StellaClark attrs) = StellaClark <$> runMessage msg attrs
