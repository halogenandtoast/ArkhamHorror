module Arkham.Types.Investigator.Cards.DianaStanley where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype DianaStanley = DianaStanley InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

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

instance (InvestigatorRunner env) => RunMessage env DianaStanley where
  runMessage msg (DianaStanley attrs) = DianaStanley <$> runMessage msg attrs
