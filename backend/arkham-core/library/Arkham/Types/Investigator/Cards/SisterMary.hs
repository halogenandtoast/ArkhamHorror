module Arkham.Types.Investigator.Cards.SisterMary where

import Arkham.Prelude

import Arkham.Types.Investigator.Attrs

newtype SisterMary = SisterMary InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

sisterMary :: SisterMary
sisterMary = SisterMary $ baseAttrs
  "07001"
  "Sister Mary"
  Guardian
  Stats
    { health = 5
    , sanity = 9
    , willpower = 4
    , intellect = 2
    , combat = 3
    , agility = 3
    }
  [Believer, Blessed]

instance (InvestigatorRunner env) => RunMessage env SisterMary where
  runMessage msg (SisterMary attrs) = SisterMary <$> runMessage msg attrs
