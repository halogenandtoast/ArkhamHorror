module Arkham.Types.Investigator.Cards.MinhThiPhan where


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype MinhThiPhan = MinhThiPhan InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env MinhThiPhan where
  getModifiersFor source target (MinhThiPhan attrs) =
    getModifiersFor source target attrs

minhThiPhan :: MinhThiPhan
minhThiPhan = MinhThiPhan $ baseAttrs
  "03002"
  "Minh Thi Phan"
  Seeker
  Stats
    { health = 7
    , sanity = 7
    , willpower = 4
    , intellect = 4
    , combat = 2
    , agility = 2
    }
  [Assistant]

instance ActionRunner env => HasActions env MinhThiPhan where
  getActions i window (MinhThiPhan attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env MinhThiPhan where
  runMessage msg (MinhThiPhan attrs) = MinhThiPhan <$> runMessage msg attrs
