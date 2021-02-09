module Arkham.Types.Investigator.Cards.FinnEdwards where


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype FinnEdwards = FinnEdwards InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env FinnEdwards where
  getModifiersFor source target (FinnEdwards attrs) =
    getModifiersFor source target attrs

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

instance ActionRunner env => HasActions env FinnEdwards where
  getActions i window (FinnEdwards attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env FinnEdwards where
  runMessage msg (FinnEdwards attrs) = FinnEdwards <$> runMessage msg attrs
