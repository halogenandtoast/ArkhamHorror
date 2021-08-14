module Arkham.Types.Investigator.Cards.FatherMateo where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
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

instance InvestigatorRunner env => HasAbilities env FatherMateo where
  getAbilities i window (FatherMateo attrs) = getAbilities i window attrs

instance (InvestigatorRunner env) => RunMessage env FatherMateo where
  runMessage msg (FatherMateo attrs) = FatherMateo <$> runMessage msg attrs
