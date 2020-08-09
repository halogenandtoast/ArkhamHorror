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

instance (InvestigatorRunner env) => RunMessage env LukeRobinson where
  runMessage msg i@(LukeRobinson attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> LukeRobinson <$> runMessage msg attrs
