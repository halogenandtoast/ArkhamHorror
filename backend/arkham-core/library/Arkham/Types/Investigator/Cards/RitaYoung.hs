module Arkham.Types.Investigator.Cards.RitaYoung where

import Arkham.Prelude

import Arkham.Types.Investigator.Attrs

newtype RitaYoung = RitaYoung InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

ritaYoung :: RitaYoung
ritaYoung = RitaYoung $ baseAttrs
  "05005"
  "Rita Young"
  Survivor
  Stats
    { health = 9
    , sanity = 5
    , willpower = 3
    , intellect = 2
    , combat = 3
    , agility = 5
    }
  [Miskatonic]

instance (InvestigatorRunner env) => RunMessage env RitaYoung where
  runMessage msg (RitaYoung attrs) = RitaYoung <$> runMessage msg attrs
