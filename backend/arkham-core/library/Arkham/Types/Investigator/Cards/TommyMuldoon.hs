module Arkham.Types.Investigator.Cards.TommyMuldoon where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype TommyMuldoon = TommyMuldoon InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env TommyMuldoon where
  getModifiersFor source target (TommyMuldoon attrs) =
    getModifiersFor source target attrs

tommyMuldoon :: TommyMuldoon
tommyMuldoon = TommyMuldoon $ baseAttrs
  "06001"
  "Tommy Muldoon"
  Guardian
  Stats
    { health = 8
    , sanity = 6
    , willpower = 3
    , intellect = 3
    , combat = 4
    , agility = 2
    }
  [Police, Warden]

instance InvestigatorRunner env => HasAbilities env TommyMuldoon where
  getAbilities i window (TommyMuldoon attrs) = getAbilities i window attrs

instance (InvestigatorRunner env) => RunMessage env TommyMuldoon where
  runMessage msg (TommyMuldoon attrs) = TommyMuldoon <$> runMessage msg attrs
