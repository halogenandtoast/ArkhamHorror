module Arkham.Types.Investigator.Cards.CarolynFern where


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype CarolynFern = CarolynFern InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env CarolynFern where
  getModifiersFor source target (CarolynFern attrs) =
    getModifiersFor source target attrs

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

instance ActionRunner env => HasActions env CarolynFern where
  getActions i window (CarolynFern attrs) = getActions i window attrs

instance InvestigatorRunner env => RunMessage env CarolynFern where
  runMessage msg (CarolynFern attrs) = CarolynFern <$> runMessage msg attrs
