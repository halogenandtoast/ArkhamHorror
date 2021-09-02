module Arkham.Types.Investigator.Cards.SilasMarsh where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype SilasMarsh = SilasMarsh InvestigatorAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

silasMarsh :: SilasMarsh
silasMarsh = SilasMarsh $ baseAttrs
  "98013"
  "Silas Marsh"
  Survivor
  Stats
    { health = 9
    , sanity = 5
    , willpower = 2
    , intellect = 2
    , combat = 4
    , agility = 4
    }
  [Drifter]

instance (InvestigatorRunner env) => RunMessage env SilasMarsh where
  runMessage msg (SilasMarsh attrs) = SilasMarsh <$> runMessage msg attrs
