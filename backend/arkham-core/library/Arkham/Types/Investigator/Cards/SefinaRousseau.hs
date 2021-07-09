module Arkham.Types.Investigator.Cards.SefinaRousseau where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype SefinaRousseau = SefinaRousseau InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env SefinaRousseau where
  getModifiersFor source target (SefinaRousseau attrs) =
    getModifiersFor source target attrs

sefinaRousseau :: SefinaRousseau
sefinaRousseau = SefinaRousseau $ baseAttrs
  "03003"
  "Sefina Rousseau"
  Rogue
  Stats
    { health = 5
    , sanity = 9
    , willpower = 4
    , intellect = 2
    , combat = 2
    , agility = 4
    }
  [Artist]

instance HasActions env SefinaRousseau where
  getActions i window (SefinaRousseau attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env SefinaRousseau where
  runMessage msg (SefinaRousseau attrs) =
    SefinaRousseau <$> runMessage msg attrs
