module Arkham.Types.Investigator.Cards.AkachiOnyele where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype AkachiOnyele = AkachiOnyele InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env AkachiOnyele where
  getModifiersFor source target (AkachiOnyele attrs) =
    getModifiersFor source target attrs

akachiOnyele :: AkachiOnyele
akachiOnyele = AkachiOnyele $ baseAttrs
  "03004"
  "Akachi Onyele"
  Mystic
  Stats
    { health = 6
    , sanity = 8
    , willpower = 5
    , intellect = 2
    , combat = 3
    , agility = 3
    }
  [Sorcerer]

instance HasActions env AkachiOnyele where
  getActions i window (AkachiOnyele attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env AkachiOnyele where
  runMessage msg (AkachiOnyele attrs) = AkachiOnyele <$> runMessage msg attrs
