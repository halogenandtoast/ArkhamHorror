module Arkham.Types.Investigator.Cards.AmandaSharpe where

import Arkham.Prelude

import Arkham.Types.Investigator.Attrs

newtype AmandaSharpe = AmandaSharpe InvestigatorAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env AmandaSharpe where
  getModifiersFor _ _ _ = pure []

amandaSharpe :: AmandaSharpe
amandaSharpe = AmandaSharpe $ baseAttrs
  "07002"
  "Amanda Sharpe"
  Seeker
  Stats
    { health = 7
    , sanity = 7
    , willpower = 2
    , intellect = 2
    , combat = 2
    , agility = 2
    }
  [Miskatonic, Scholar]

instance InvestigatorRunner env => RunMessage env AmandaSharpe where
  runMessage msg (AmandaSharpe attrs) = AmandaSharpe <$> runMessage msg attrs
