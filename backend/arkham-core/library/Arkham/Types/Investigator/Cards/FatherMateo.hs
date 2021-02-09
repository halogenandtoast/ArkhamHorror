module Arkham.Types.Investigator.Cards.FatherMateo where


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype FatherMateo = FatherMateo InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env FatherMateo where
  getModifiersFor source target (FatherMateo attrs) =
    getModifiersFor source target attrs

fatherMateo :: FatherMateo
fatherMateo = FatherMateo $ baseAttrs
  "04004"
  "Father Mateo"
  Mystic
  Stats
    { health = 6
    , sanity = 8
    , willpower = 4
    , intellect = 3
    , combat = 2
    , agility = 3
    }
  [Believer, Warden]

instance ActionRunner env => HasActions env FatherMateo where
  getActions i window (FatherMateo attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env FatherMateo where
  runMessage msg (FatherMateo attrs) = FatherMateo <$> runMessage msg attrs
