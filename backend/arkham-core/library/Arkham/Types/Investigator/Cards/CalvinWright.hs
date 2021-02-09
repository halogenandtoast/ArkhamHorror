module Arkham.Types.Investigator.Cards.CalvinWright where


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype CalvinWright = CalvinWright InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env CalvinWright where
  getModifiersFor source target (CalvinWright attrs) =
    getModifiersFor source target attrs

calvinWright :: CalvinWright
calvinWright = CalvinWright $ baseAttrs
  "04005"
  "Calvin Wright"
  Survivor
  Stats
    { health = 6
    , sanity = 6
    , willpower = 0
    , intellect = 0
    , combat = 0
    , agility = 0
    }
  [Cursed, Drifter]

instance ActionRunner env => HasActions env CalvinWright where
  getActions i window (CalvinWright attrs) = getActions i window attrs

instance InvestigatorRunner env => RunMessage env CalvinWright where
  runMessage msg (CalvinWright attrs) = CalvinWright <$> runMessage msg attrs
