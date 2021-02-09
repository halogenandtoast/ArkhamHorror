module Arkham.Types.Investigator.Cards.RitaYoung where


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype RitaYoung = RitaYoung InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env RitaYoung where
  getModifiersFor source target (RitaYoung attrs) =
    getModifiersFor source target attrs

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

instance ActionRunner env => HasActions env RitaYoung where
  getActions i window (RitaYoung attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env RitaYoung where
  runMessage msg (RitaYoung attrs) = RitaYoung <$> runMessage msg attrs
