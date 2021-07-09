module Arkham.Types.Investigator.Cards.DianaStanley where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype DianaStanley = DianaStanley InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env DianaStanley where
  getModifiersFor source target (DianaStanley attrs) =
    getModifiersFor source target attrs

dianaStanley :: DianaStanley
dianaStanley = DianaStanley $ baseAttrs
  "05004"
  "Diana Stanley"
  Mystic
  Stats
    { health = 7
    , sanity = 7
    , willpower = 1
    , intellect = 3
    , combat = 3
    , agility = 3
    }
  [Cultist, SilverTwilight]

instance HasActions env DianaStanley where
  getActions i window (DianaStanley attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env DianaStanley where
  runMessage msg (DianaStanley attrs) = DianaStanley <$> runMessage msg attrs
