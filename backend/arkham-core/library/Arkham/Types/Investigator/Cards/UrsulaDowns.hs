module Arkham.Types.Investigator.Cards.UrsulaDowns where

import Arkham.Prelude

import Arkham.Types.Investigator.Attrs

newtype UrsulaDowns = UrsulaDowns InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

ursulaDowns :: UrsulaDowns
ursulaDowns = UrsulaDowns $ baseAttrs
  "04002"
  "Ursula Downs"
  Seeker
  Stats
    { health = 7
    , sanity = 7
    , willpower = 3
    , intellect = 4
    , combat = 1
    , agility = 4
    }
  [Wayfarer]

instance (InvestigatorRunner env) => RunMessage env UrsulaDowns where
  runMessage msg (UrsulaDowns attrs) = UrsulaDowns <$> runMessage msg attrs
