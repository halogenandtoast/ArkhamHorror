module Arkham.Types.Investigator.Cards.LukeRobinson where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype LukeRobinson = LukeRobinson InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env LukeRobinson where
  getModifiersFor source target (LukeRobinson attrs) =
    getModifiersFor source target attrs

lukeRobinson :: LukeRobinson
lukeRobinson = LukeRobinson $ baseAttrs
  "06004"
  "Luke Robinson"
  Mystic
  Stats
    { health = 5
    , sanity = 9
    , willpower = 4
    , intellect = 3
    , combat = 2
    , agility = 3
    }
  [Dreamer, Drifter, Wayfarer]

instance HasActions env LukeRobinson where
  getActions i window (LukeRobinson attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env LukeRobinson where
  runMessage msg (LukeRobinson attrs) = LukeRobinson <$> runMessage msg attrs
