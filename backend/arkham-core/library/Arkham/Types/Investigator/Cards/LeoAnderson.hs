module Arkham.Types.Investigator.Cards.LeoAnderson where


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype LeoAnderson = LeoAnderson InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env LeoAnderson where
  getModifiersFor source target (LeoAnderson attrs) =
    getModifiersFor source target attrs

leoAnderson :: LeoAnderson
leoAnderson = LeoAnderson $ baseAttrs
  "04001"
  "Leo Anderson"
  Guardian
  Stats
    { health = 8
    , sanity = 6
    , willpower = 4
    , intellect = 3
    , combat = 4
    , agility = 1
    }
  [Veteran, Wayfarer]

instance ActionRunner env => HasActions env LeoAnderson where
  getActions i window (LeoAnderson attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env LeoAnderson where
  runMessage msg (LeoAnderson attrs) = LeoAnderson <$> runMessage msg attrs
