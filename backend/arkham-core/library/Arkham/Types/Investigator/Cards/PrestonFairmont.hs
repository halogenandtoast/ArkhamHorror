module Arkham.Types.Investigator.Cards.PrestonFairmont where

import Arkham.Prelude

import Arkham.Types.Investigator.Attrs

newtype PrestonFairmont = PrestonFairmont InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

prestonFairmont :: PrestonFairmont
prestonFairmont = PrestonFairmont $ baseAttrs
  "05003"
  "Preston Fairmont"
  Rogue
  Stats
    { health = 7
    , sanity = 7
    , willpower = 1
    , intellect = 1
    , combat = 1
    , agility = 1
    }
  [SilverTwilight, Socialite]

instance (InvestigatorRunner env) => RunMessage env PrestonFairmont where
  runMessage msg (PrestonFairmont attrs) =
    PrestonFairmont <$> runMessage msg attrs
