module Arkham.Types.Investigator.Cards.StellaClark where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype StellaClark = StellaClark InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env StellaClark where
  getModifiersFor source target (StellaClark attrs) =
    getModifiersFor source target attrs

stellaClark :: StellaClark
stellaClark = StellaClark $ baseAttrs
  "60501"
  "Stella Clark"
  Survivor
  Stats
    { health = 8
    , sanity = 8
    , willpower = 3
    , intellect = 2
    , combat = 3
    , agility = 4
    }
  [Chosen, Civic]

instance HasActions env StellaClark where
  getActions i window (StellaClark attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env StellaClark where
  runMessage msg (StellaClark attrs) = StellaClark <$> runMessage msg attrs
