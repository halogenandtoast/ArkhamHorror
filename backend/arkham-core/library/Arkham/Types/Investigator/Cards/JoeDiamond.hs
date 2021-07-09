module Arkham.Types.Investigator.Cards.JoeDiamond where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype JoeDiamond = JoeDiamond InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env JoeDiamond where
  getModifiersFor source target (JoeDiamond attrs) =
    getModifiersFor source target attrs

joeDiamond :: JoeDiamond
joeDiamond = JoeDiamond $ baseAttrs
  "05002"
  "Joe Diamond"
  Seeker
  Stats
    { health = 8
    , sanity = 6
    , willpower = 2
    , intellect = 4
    , combat = 4
    , agility = 2
    }
  [Detective]

instance HasActions env JoeDiamond where
  getActions i window (JoeDiamond attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env JoeDiamond where
  runMessage msg (JoeDiamond attrs) = JoeDiamond <$> runMessage msg attrs
