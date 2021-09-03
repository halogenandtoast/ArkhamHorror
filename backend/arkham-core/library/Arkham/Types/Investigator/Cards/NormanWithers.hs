module Arkham.Types.Investigator.Cards.NormanWithers where

import Arkham.Prelude

import Arkham.Types.Investigator.Attrs

newtype NormanWithers = NormanWithers InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

normanWithers :: NormanWithers
normanWithers = NormanWithers $ baseAttrs
  "98007"
  "Norman Withers"
  Seeker
  Stats
    { health = 6
    , sanity = 8
    , willpower = 4
    , intellect = 5
    , combat = 2
    , agility = 1
    }
  [Miskatonic]

instance (InvestigatorRunner env) => RunMessage env NormanWithers where
  runMessage msg (NormanWithers attrs) = NormanWithers <$> runMessage msg attrs
