module Arkham.Types.Investigator.Cards.CarolynFern where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype CarolynFern = CarolynFern InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

carolynFern :: CarolynFern
carolynFern = CarolynFern $ baseAttrs
  "05001"
  "Carolyn Fern"
  Guardian
  Stats
    { health = 6
    , sanity = 9
    , willpower = 3
    , intellect = 4
    , combat = 2
    , agility = 2
    }
  [Medic]

instance InvestigatorRunner env => RunMessage env CarolynFern where
  runMessage msg (CarolynFern attrs) = CarolynFern <$> runMessage msg attrs
