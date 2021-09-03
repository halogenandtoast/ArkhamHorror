module Arkham.Types.Investigator.Cards.MandyThompson where

import Arkham.Prelude

import Arkham.Types.Investigator.Attrs

newtype MandyThompson = MandyThompson InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

mandyThompson :: MandyThompson
mandyThompson = MandyThompson $ baseAttrs
  "06002"
  "Mandy Thompson"
  Seeker
  Stats
    { health = 6
    , sanity = 8
    , willpower = 3
    , intellect = 5
    , combat = 1
    , agility = 3
    }
  [Assistant, Scholar]

instance (InvestigatorRunner env) => RunMessage env MandyThompson where
  runMessage msg (MandyThompson attrs) = MandyThompson <$> runMessage msg attrs
