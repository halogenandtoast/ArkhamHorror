module Arkham.Types.Investigator.Cards.AmandaSharpe where


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype AmandaSharpe = AmandaSharpe InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env AmandaSharpe where
  getModifiersFor source target (AmandaSharpe attrs) =
    getModifiersFor source target attrs

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

instance ActionRunner env => HasActions env AmandaSharpe where
  getActions i window (AmandaSharpe attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env AmandaSharpe where
  runMessage msg (AmandaSharpe attrs) = AmandaSharpe <$> runMessage msg attrs
