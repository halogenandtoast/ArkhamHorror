{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.LukeRobinson where

import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype LukeRobinson = LukeRobinson Attrs
  deriving newtype (Show, ToJSON, FromJSON)

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

instance ActionRunner env => HasActions env LukeRobinson where
  getActions i window (LukeRobinson attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env LukeRobinson where
  runMessage msg i@(LukeRobinson attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId -> pure i
    _ -> LukeRobinson <$> runMessage msg attrs
