module Arkham.Types.Investigator.Cards.MarkHarrigan where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype MarkHarrigan = MarkHarrigan InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env MarkHarrigan where
  getModifiersFor source target (MarkHarrigan attrs) =
    getModifiersFor source target attrs

markHarrigan :: MarkHarrigan
markHarrigan = MarkHarrigan $ baseAttrs
  "03001"
  "Mark Harrigan"
  Guardian
  Stats
    { health = 9
    , sanity = 5
    , willpower = 3
    , intellect = 2
    , combat = 5
    , agility = 3
    }
  [Veteran]

instance HasActions env MarkHarrigan where
  getActions i window (MarkHarrigan attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env MarkHarrigan where
  runMessage msg (MarkHarrigan attrs) = MarkHarrigan <$> runMessage msg attrs
