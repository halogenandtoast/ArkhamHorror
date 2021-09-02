module Arkham.Types.Investigator.Cards.FinnEdwards where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype FinnEdwards = FinnEdwards InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

finnEdwards :: FinnEdwards
finnEdwards = FinnEdwards $ baseAttrs
  "04003"
  "Finn Edwards"
  Rogue
  Stats
    { health = 7
    , sanity = 7
    , willpower = 1
    , intellect = 4
    , combat = 3
    , agility = 4
    }
  [Criminal]

instance (InvestigatorRunner env) => RunMessage env FinnEdwards where
  runMessage msg (FinnEdwards attrs) = FinnEdwards <$> runMessage msg attrs
